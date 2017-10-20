# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/ice/csim4/camice.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/ice/csim4/camice.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/ice/csim4/camice.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/ice/csim4/camice.F90" 2

subroutine camice(srf_state,srfflx)

!----------------------------------------------------------------------- 
! 
! Purpose: 
! CAM sea ice surface fluxes.
!
! Method: 
! 
! Author:
! 
!-----------------------------------------------------------------------
!
! $Id: camice.F90,v 1.1.4.3 2002/06/15 13:50:09 erik Exp $
! $Author: erik $
!
!-----------------------------------------------------------------------

  use shr_kind_mod, only: r8 => shr_kind_r8
  use ppgrid
  use pspect
  use ice_data
  use comsrf, only: surface_state,srfflx_parm,icefrac,snowhice,sicthk, &
	tsice,asdirice,asdifice,aldirice,aldifice
  use phys_grid, only: get_ncols_p, get_rlat_all_p, get_rlon_all_p
  use time_manager, only: get_nstep, get_step_size, get_curr_calday
  use ice_dh, only:prognostic_icesnow
  implicit none
!
! Input/Output arguments
!
   type(surface_state), intent(inout), dimension(begchunk:endchunk) :: srf_state
   type(srfflx_parm), intent(inout), dimension(begchunk:endchunk) :: srfflx


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
# 39 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/ice/csim4/camice.F90" 2

!---------------------------Local variables-----------------------------
  integer :: nstep          ! current timestep number
  integer :: dtime          ! timestep size [seconds]
  real(r8) rtime            ! calendar day for next timestep
  real(r8) lats(pcols)            ! 
  real(r8) lons(pcols)            ! 
  real(r8) cdaynext         ! calendar day for next timestep
  real(r8) cosznext(pcols)  ! cosine solar zenith angle next timestep
  integer ncol              ! number of columns in chunk
  integer c             ! chunk index
  integer idum1,idum2,idum3,idum4,i ! temporary variables
  real(r8) snowfall(pcols,begchunk:endchunk)  ! total snowfall rate
!-----------------------------------------------------------------------
!
! Calendar day for next time step
!
  call t_startf ('camice_st')
  nstep = get_nstep()
  dtime = get_step_size()
  rtime=dtime
  cdaynext = get_curr_calday(offset=dtime)
!
! set up snowfall here so it doesn't have to be private in the omp call
!
  do c=begchunk,endchunk
     ncol = get_ncols_p(c)
     do i = 1,ncol
	if (prognostic_icesnow) then
           snowfall(i,c)=srf_state(c)%precsc(i)+srf_state(c)%precsl(i)
        else
           snowfall(i,c)=0.	
        end if
     end do
  end do
  call t_stopf ('camice_st')

!$OMP PARALLEL DO PRIVATE (C, NCOL, LATS, LONS, COSZNEXT,I)

  do c=begchunk,endchunk
     ncol = get_ncols_p(c)

! Sea ice surface fluxes and temperatures

     call seaice (c, ncol, rtime, icefrac(1,c), tsice(1,c), &
                  sicthk(1,c), snowhice(1,c), srf_state(c)%ubot, &
                     srf_state(c)%vbot, srf_state(c)%tbot, &
                  srf_state(c)%qbot, srf_state(c)%thbot, srf_state(c)%zbot, &
                     srf_state(c)%pbot ,srf_state(c)%flwds, &
                  srf_state(c)%sols, srf_state(c)%soll, srf_state(c)%solsd, &
                     srf_state(c)%solld, asdirice(1,c), &
                  aldirice(1,c), asdifice(1,c), aldifice(1,c), &
    	             snowfall(1,c), srf_state(c)%tssub, &
                  srfflx(c)%cflx, srfflx(c)%wsx, srfflx(c)%wsy, &
                     srfflx(c)%ts, srfflx(c)%shf, &
         	  srfflx(c)%lhf, srfflx(c)%lwup, srfflx(c)%tref)
		 
!
! Albedos for next time step 
!
! Note the total albedo here that is returned to the atmosphere 
! model is based on a weighted sum of the albedo over ice and ocean
! using fractional areas from this time step. The absorbed shortwave over
! sea ice in the next step uses ice albedos that are saved at there
! present value but with a NEW fractional area that is input prior to 
! the next time through the sea ice model.  Hence
! there is a time step mismatch in the absorbed solar over sea ice. 
! CCSM would not allow such a thing, but here we are specifying sst, 
! over the ocean fraction anyway so it doesn't really matter. 

     call get_rlat_all_p(c, ncol, lats)
     call get_rlon_all_p(c, ncol, lons)
     call zenith (cdaynext, lats, lons, cosznext, ncol)
     call albice(c,ncol, &
                 srf_state(c)%tbot,snowhice(1,c),cosznext, &
                 srfflx(c)%asdir, srfflx(c)%aldir, &
                 srfflx(c)%asdif, srfflx(c)%aldif)
!
! save off ice albedos for sea ice routine per email Bitz.
! I should note that I made one change to the "physics" from John's
! original fracice implementation. John had the absorbed solar by the
! sea ice equal to the gridcell average.  This is pretty far off when
! the sea ice fraction is small. I realize that it is standard practise
! in many models, but it doesn't have to be.  Therefore I have compute a
! special srfrad over ice and I send the ice albedos to the restart
! file.
!
     do i = 1,ncol
        asdirice(i,c)=srfflx(c)%asdir(i)
        aldirice(i,c)=srfflx(c)%aldir(i)
        asdifice(i,c)=srfflx(c)%asdif(i)
        aldifice(i,c)=srfflx(c)%aldif(i)
     end do

  end do

  return
end subroutine camice
