# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/ocnsice/dom/camoce.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/ocnsice/dom/camoce.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/ocnsice/dom/camoce.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/ocnsice/dom/camoce.F90" 2

!-----------------------------------------------------------------------
!
! Purpose:
! CAM ocean and sea ice surface fluxes.
!
! Method:
!
! Author:
!
!-----------------------------------------------------------------------
!
! $Id: camoce.F90,v 1.1.2.3 2002/06/15 13:48:55 erik Exp $
! $Author: erik $
!
!-----------------------------------------------------------------------

subroutine camoce(srf_state,srfflx)

    use shr_kind_mod, only: r8 => shr_kind_r8
    use ppgrid
    use pspect
    use comsrf,       only: surface_state,srfflx_parm,ocnfrac
    use phys_grid,    only: get_ncols_p, get_rlat_all_p, get_rlon_all_p
    use sst_data,     only: sstan
    use time_manager, only: get_nstep, get_step_size, get_curr_calday

    implicit none

    type(surface_state), intent(inout) :: srf_state(begchunk:endchunk)
    type(srfflx_parm),   intent(inout) :: srfflx(begchunk:endchunk)


# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/comctl.h" 1
!----------------------------------------------------------------------- 
! 
! Purpose: Model control variables
! 
! Author: CCM Core Group
! 
!-----------------------------------------------------------------------
!! (wh 2003.04.30)
!! (wh 2003.12.27)

      common /comctl/ itsst   ,nsrest  ,iradsw  ,iradlw  ,iradae
      common /comctl/ nrefrq
      common /comctl/ anncyc  ,nlend   ,nlres   ,nlhst   ,lbrnch
      common /comctl/ aeres   ,ozncyc  ,sstcyc  ,icecyc
      common /comctl/ adiabatic,flxave
      common /comctl/ trace_gas, trace_test1,trace_test2, trace_test3
!!    common /comctl/ readtrace,ideal_phys, nsplit, iord, jord, kord, use_eta, aqua_planet
      common /comctl/ readtrace,ideal_phys,                                    aqua_planet
      common /comctl/ doRamp_ghg, doRamp_so4, doRamp_scon, fullgrid, doIPCC_so4, &
                      doCmip5_scon,doCmip5_ghg  !!(wh)
      common /comctl/ print_step_cost
      common /comctl/ doabsems, dosw, dolw, indirect

!!    common /comctl_r8/ divdampn, precc_thresh, precl_thresh
      common /comctl_r8/           precc_thresh, precl_thresh

      integer itsst             ! Sea surf. temp. update freq. (iters)
      integer nsrest            ! Restart flag
      integer iradsw            ! Iteration freq. for shortwave radiation
      integer iradlw            ! Iteration freq. for longwave radiation
      integer iradae            ! Iteration freq. for absorptivity/emissivity
      integer nrefrq            ! Restart write freq.

! f-v dynamics specific
! _ord = 1: first order upwind
! _ord = 2: 2nd order van Leer (Lin et al 1994)
! _ord = 3: standard PPM 
! _ord = 4: enhanced PPM (default)
!!      integer nsplit            ! Lagrangian time splits (Lin-Rood only)
!!      integer iord              ! scheme to be used in E-W direction
!!      integer jord              ! scheme to be used in N-S direction
!!      integer kord              ! scheme to be used for vertical mapping
!!      logical use_eta           ! Flag to use a's and b's set by dynamics/lr/set_eta.F90

      logical aqua_planet       ! Flag to run model in "aqua planet" mode

      logical anncyc            ! true => do annual cycle (otherwise perpetual)
      logical nlend             ! true => end of run
      logical nlres             ! true => continuation run
      logical nlhst             ! true => regeneration run
      logical lbrnch            ! true => branch run
      logical aeres             ! true => read/write a/e data to/from restart file
      logical ozncyc            ! true => cycle ozone dataset
      logical sstcyc            ! true => cycle sst dataset
      logical icecyc            ! true => cycle ice fraction dataset
      logical adiabatic         ! true => no physics
      logical ideal_phys        ! true => run "idealized" model configuration
      logical flxave            ! true => send to coupler only on radiation time steps

      logical trace_gas         ! true => turn on greenhouse gas code
      logical trace_test1       ! true => turn on test tracer code with 1 tracer
      logical trace_test2       ! true => turn on test tracer code with 2 tracers
      logical trace_test3       ! true => turn on test tracer code with 3 tracers
      logical readtrace         ! true => obtain initial tracer data from IC file

      logical doRamp_ghg        ! true => turn on ramping for ghg
      logical doRamp_so4        ! true => turn on ramping for so4
      logical doRamp_scon       ! true => turn on ramping for scon
      logical doIPCC_so4        ! true => turn on IPCC scenario for so4  !!(wh) 
      logical doCmip5_scon      !
      logical doCmip5_ghg       ! ljli2010-08-12
      logical fullgrid          ! true => no grid reduction towards poles

      logical print_step_cost   ! true => print per-timestep cost info

      logical doabsems          ! True => abs/emiss calculation this timestep
      logical dosw              ! True => shortwave calculation this timestep
      logical dolw              ! True => longwave calculation this timestep
      logical indirect          ! True => include indirect radiative effects of sulfate aerosols

!!    real(r8) divdampn         ! Number of days to invoke divergence damper
      real(r8) precc_thresh     ! Precipitation threshold for PRECCINT and PRECCFRQ
      real(r8) precl_thresh     ! Precipitation threshold for PRECLINT and PRECLFRQ
# 36 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/ocnsice/dom/camoce.F90" 2

    integer nstep               ! current timestep number
    integer dtime               ! timestep size [seconds]
    real(r8) cdaynext           ! calendar day for next timestep
    real(r8) clat(pcols)        ! current latitudes(radians)
    real(r8) clon(pcols)        ! current longitudes(radians)
    real(r8) cosznext(pcols)    ! cosine solar zenith angle next timestep
    integer ncol                ! number of columns in chunk
    integer lchnk               ! chunk index

    !
    ! Calendar day for next time step
    !
    nstep = get_nstep()
    dtime = get_step_size()
    cdaynext = get_curr_calday(offset=dtime)

!$OMP PARALLEL DO PRIVATE (LCHNK, NCOL, CLAT, CLON, COSZNEXT)

    do lchnk = begchunk, endchunk
        ncol = get_ncols_p(lchnk)
        !
        ! Update ts and for open ocean
        !
        if (anncyc .and. mod(nstep, itsst) == 0) then
            call sstan(lchnk, ncol, ocnfrac(1,lchnk), srfflx(lchnk)%ts)
        end if
        !
        ! Ocean surface fluxes and temperatures
        !
        call srfoce(lchnk, ncol, &
                    ocnfrac(1,lchnk)     , srf_state(lchnk)%ubot,  &
                    srf_state(lchnk)%vbot, srf_state(lchnk)%tbot,  &
                    srf_state(lchnk)%qbot, srf_state(lchnk)%thbot, &
                    srf_state(lchnk)%zbot, srf_state(lchnk)%pbot,  &
                    srfflx(lchnk)%cflx, &
                    srfflx(lchnk)%wsx   , srfflx(lchnk)%wsy ,&
                    srfflx(lchnk)%ts    , srfflx(lchnk)%shf ,&
                    srfflx(lchnk)%lhf   , srfflx(lchnk)%lwup,&
                    srfflx(lchnk)%tref)
        !
        ! Albedos for next time step
        !
        call get_rlat_all_p(lchnk, ncol, clat)
        call get_rlon_all_p(lchnk, ncol, clon)
        call zenith(cdaynext, clat, clon, cosznext, ncol)

        call albocean(lchnk, ncol, cosznext, &
                      srfflx(lchnk)%asdir, srfflx(lchnk)%aldir, &
                      srfflx(lchnk)%asdif, srfflx(lchnk)%aldif)
    end do

    return
end subroutine camoce
