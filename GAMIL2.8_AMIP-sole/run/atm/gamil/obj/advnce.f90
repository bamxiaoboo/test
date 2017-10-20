# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/advnce.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/advnce.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/advnce.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/advnce.F90" 2

subroutine advnce(state)
    !-----------------------------------------------------------------------
    !
    ! Purpose:
    ! Advance time information
    !
    ! Method:
    !
    ! Author: CCM1, CMS Contact: J. Truesdale
    !
    !-----------------------------------------------------------------------
    !! (wh 2003.12.27)

    use shr_kind_mod,        only: r8 => shr_kind_r8
    use chemistry,           only: chem_time_interp
    use so4bnd
    use so4bnd_IPCC                                     ! added by WAN Hui
    use ramp_so4_mod
    use time_manager,        only: get_nstep

    use prescribed_aerosols, only:aerint                ! added by SHI Xiangjun 2009-0311
    use physics_types,       only: physics_state        ! added by SHI Xiangjun 2009-0311
    use ppgrid,              only: begchunk, endchunk   ! added by SHI Xiangjun 2009-0311


    use ice_data,            only: iceint
    use sst_data,            only: sstint


    implicit none


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
# 36 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/advnce.F90" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/comlun.h" 1
!----------------------------------------------------------------------- 
! 
! Purpose: Logical unit numbers and related variables
!
! Author: CCM Core Group
! 
!-----------------------------------------------------------------------

      common /comlun/ nsds    ,nrg     ,nrg2
      common /comlun/ ncid_ini,ncid_oz ,ncid_sst, ncid_trc
      common /comlun/ luhrest

      integer nsds       ! restart dataset unit
      integer nrg        ! master regeneration dataset unit
      integer nrg2       ! abs/ems regeneration dataset units
      integer ncid_ini   ! initial dataset
      integer ncid_oz    ! ozone dataset
      integer ncid_trc   ! greenhouse gas tracer dataset
      integer ncid_sst   ! sst dataset
      integer luhrest    ! history restart unit
# 37 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/advnce.F90" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/RK_or_MG.h" 1
 
  character(len=2)  RK_or_MG
  parameter (RK_or_MG='MG')

# 38 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/advnce.F90" 2
    !
    !-----------------------------------------------------------------------
    !
    ! Local workspace
    type(physics_state), intent(in), dimension(begchunk:endchunk) :: state !!! sxj-2009-0311
    !
    integer :: nstep             ! current timestep number
    !-----------------------------------------------------------------------

    nstep = get_nstep()
    !
    ! Determine whether it is time for a shortwave or longwave radiation
    ! calculation
    !
    dosw = nstep.eq.0 .or. iradsw.eq.1 .or. (mod(nstep-1,iradsw).eq.0 .and. nstep.ne.1)
    dolw = nstep.eq.0 .or. iradlw.eq.1 .or. (mod(nstep-1,iradlw).eq.0 .and. nstep.ne.1)
    !
    ! Determine whether it is time for an absorptivity/emissivity calculation
    !
    doabsems = nstep.eq.0 .or. iradae.eq.1 .or. (mod(nstep-1,iradae).eq.0 .and. nstep.ne.1)
    aeres = (mod(nstep,iradae).ne.0)
    !
    ! Update ozone data on shortwave or longwave time step.
    ! Note that the ozone data is not needed on a longwave time step unless the
    ! absorptivities are being updated ("doabsems").
    !
    if (dosw .or. dolw) then
        call oznint
    end if
    if (RK_or_MG=='MG')then  !sxj----
        call aerint(state)
    endif
    !write(*,*) "advnce.F90-line67"
    !call endrun

    !
    ! Time interpolate sst data
    !






    call sstint ()
    call iceint ()


    !
    ! Ramping ghg if appropriate
    !
    if (doRamp_ghg) call ramp_ghg
    if (doCmip5_ghg) call cmip5_ghg
    !
    ! Ramp solar constant if appropraite
    !
    if (doRamp_scon) call ramp_scon
    if (doCmip5_scon) call cmip5_scon
    !
    ! Time interpolate for chemistry, if appropriate
    !
    if (trace_gas) call chem_time_interp
    !
    ! sulfate aerosols
    !
    if ( doRamp_so4 ) then
        call ramp_so4
        call sulfint
    end if

    if (doIPCC_so4) call sulfint_IPCC

end subroutine advnce
