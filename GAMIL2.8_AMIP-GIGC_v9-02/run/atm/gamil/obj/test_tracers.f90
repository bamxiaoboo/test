# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/test_tracers.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/test_tracers.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/test_tracers.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/test_tracers.F90" 2
subroutine initesttr( q3,nlon )
!----------------------------------------------------------------------- 
! 
! Purpose: 
! <Say what the routine does> 
! 
! Method: 
! <Describe the algorithm(s) used in the routine.> 
! <Also include any applicable external references.> 
! 
! Author: <Who is primarily responsible for the code> 
! 
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
! Initialize test tracers.  The test tracers are:
!
!    1) Radon, init to zero, surface fluxes from WCRP95, 5.5
!       day e-folding decay.
!    2) conserved unit tracer
!    3) ozone-like tracer, init to 1.e-9 above ~100mb, zero
!       elsewhere, re-zero the bottom level at each timestep.
! Note that:
!    o ixtrct   = index of radon advected tracer
!    o ixtrct+1 = index of conserved unit tracer
!    o ixtrct+2 = index of ozone-like tracer
!
!-------------------------Code History----------------------------------
!
! Original version:  B. Eaton, 1995
! Standardized:      T. Acker, Feb 1996
! Reviewed:
!
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8
   use pmgrid
   use tracers, only: pcnst, pnats, ixtrct
!-----------------------------------------------------------------------
   implicit none
!-----------------------------------------------------------------------

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
# 44 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/test_tracers.F90" 2
!-----------------------------------------------------------------------
!
! Output arguments:
!
   real(r8), intent(out) :: q3(plond,plev,pcnst+pnats)    ! kg tracer/kg dry air
   integer, intent(in) :: nlon
!
!--------------------------Local Variables------------------------------
!
   integer i, k                       !  loop counters
!
!-----------------------------------------------------------------------
!
!
! Initialize radon tracer to zero.
!
   if ( trace_test1 .or. trace_test2 .or. trace_test3 ) then
      do k = 1, plev
         do i = 1, nlon
            q3(i,k,ixtrct) = 0.0
         end do
      end do
   end if
!
! Initialize conserved unit tracer.
!
   if ( trace_test2 .or. trace_test3 ) then
      do k = 1, plev
         do i = 1, nlon
            q3(i,k,ixtrct+1) = 1.0
         end do
      end do
   end if
!
! Initialize strat tracer to 1.e-9 above 100mb
!
   if ( trace_test3 ) then
      do k = 1, plev
         do i = 1, nlon
            q3(i,k,ixtrct+2) = 0.0
         end do
      end do
      do k = 1, 5
         do i = 1, nlon
            q3(i,k,ixtrct+2) = 1.e-9
         end do
      end do
   end if

   return
end subroutine initesttr
!
!#######################################################################
!
subroutine rndecay( lchnk, ncol, rn, deltat, rnsnk)
!----------------------------------------------------------------------- 
! 
! Purpose: 
! <Say what the routine does> 
! 
! Method: 
! <Describe the algorithm(s) used in the routine.> 
! <Also include any applicable external references.> 
! 
! Author: <Who is primarily responsible for the code> 
! 
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
! Radon decay.
!
!-------------------------Code History----------------------------------
!
! Original version:  B. Eaton, 1995
! Standardized:      T. Acker, Feb 1996
! Reviewed:
!
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8
   use ppgrid
   implicit none
!-------------------------Arguments--------------------------------------
!
! input args
   integer, intent(in) :: lchnk                 ! chunk identifier
   integer, intent(in) :: ncol                  ! number of atmospheric columns

   real(r8), intent(in) :: rn(pcols,pver)       ! radon mixing ratio (kg/(kg moist air))
   real(r8), intent(in) :: deltat               ! time step
!
! output args
   real(r8), intent(out) :: rnsnk(pcols,pver)    ! conversion rate
!                               !              (kg rn /(s kg moist air))
!
!--------------------------Local Variables------------------------------
!
   integer i                 ! x index
   integer k                 ! z index
!
   real(r8) a                    ! lifetime
   parameter( a = 2.10e-6 )
!
!-----------------------------------------------------------------------
!
!   calculate tendencies using Euler Backward
!
   do k = 1,pver
      do i = 1,ncol
         rnsnk(i,k) = -rn(i,k)*a / (1. + a*deltat)
      end do
   end do
!
!
   return
!
end subroutine rndecay
!
!########################################################################
!
subroutine rnsfwcrp( lchnk, ncol, landfrac, flux )
!----------------------------------------------------------------------- 
! 
! Purpose: 
! <Say what the routine does> 
! 
! Method: 
! <Describe the algorithm(s) used in the routine.> 
! <Also include any applicable external references.> 
! 
! Author: <Who is primarily responsible for the code> 
! 
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
! Set surface fluxes for radon for WCRP95 RN-PB simulation.
!
!  The flux is specified non-zero over land between 60S - 70N, except
!  exclude Greenland.
!
!  Flux strength:
!  60S - 60N:  3.69e-21 kg/m^2/s
!  60N - 70N:  (3.69e-21)/2 kg/m^2/s
!
!  This land source is has been adjusted so that the total radon flux is
!  15 kg/yr for a T42 grid.
!
!-------------------------Code History----------------------------------
!
! Original version:  B. Eaton, 1995
! Standardized:      T. Acker, Feb 1996
! Reviewed:
!
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8
   use ppgrid
   use phys_grid,     only: get_rlat_all_p, get_rlon_all_p
!-----------------------------------------------------------------------
   implicit none
!--------------------------Arguments-------------------------------------
!
! Input arguments:
!
   integer, intent(in) :: lchnk           ! chunk identifier
   integer, intent(in) :: ncol            ! number of atmospheric columns

   real(r8), intent(in) :: landfrac(pcols)! landfraction
!
! Output arguments:
!
   real(r8), intent(out) :: flux(pcols)    ! specified radon flux in kg/m^2/s
!
!--------------------------Local Variables------------------------------
!
   integer i      ! loop counter

   real(r8) rlat(pcols)                  ! current latitudes(radians)
   real(r8) rlon(pcols)                  ! current longitudes(radians)
   real(r8) landflx   ! land flux
   real(r8) landflxn  ! (land flux)/2
   real(r8) rad2deg   ! convert radians to degrees
   real(r8) latdeg    ! latitude in degrees
!
!--------------------------Statement functions--------------------------
!
   logical land
   land(i) = nint(landfrac(i)).gt.0.9999_r8
!
!-----------------------------------------------------------------------
!
!
!--------------------------Parameters-----------------------------------
!
   parameter( rad2deg = 360. / 6.283185308)
!
!------------------------------------------------------------------------
!
!      landflx = 3.69e-21
   landflx = 3.7796e-21   ! rescaled so total flux is 15 kg/yr (T42)
   landflxn = landflx/2.
!
   call get_rlat_all_p(lchnk, ncol, rlat)
   call get_rlon_all_p(lchnk, ncol, rlon)
   do i = 1, ncol
!
      flux(i) = 0.
      latdeg = rlat(i) * rad2deg
      if ( latdeg .ge. -60.  .and.  latdeg .le. 60. ) then    ! 60S - 60N
         if ( land(i) ) flux(i) = landflx
      else if ( latdeg .gt. 60. .and. latdeg .le. 70 ) then   ! 60N - 70N
         if (rlon(i)*rad2deg .le. 300.0) then            ! 0 - 300E excludes Greenland
            if ( land(i) ) flux(i) = landflxn
         end if
      end if
!
   end do
!
   return
!
end subroutine rnsfwcrp
