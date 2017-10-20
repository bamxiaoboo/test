# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/inti.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/inti.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/inti.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/inti.F90" 2

subroutine inti 
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Set constants and call initialization procedures for time independent
! physics routines
! 
! Method: 
! <Describe the algorithm(s) used in the routine.> 
! <Also include any applicable external references.> 
! 
! Author: J. Rosinski
! 
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8
   use pmgrid,             only: plev, plevp, plevstd,plond,plat ! Needed for hypm passed to vd_inti
   use chemistry,          only: chem_initialize
   use ppgrid,           only: begchunk, endchunk, pcols  !! sxj--
   use physics_types,    only: physics_state,physics_state_set_grid  !1 sxj--
   use gw_drag,            only: gw_inti
   use vertical_diffusion, only: vd_inti
   use moistconvection,    only: mfinti,convection_scheme 
   use cldwat,             only: inimc
   use zm_conv,            only: zm_convi
   use zm_conv_3,          only: zm_convi_3
   use shr_const_mod,      only: shr_const_zvir, shr_const_cpwv, shr_const_rwv
   use physconst,          only: rair, cpair, cpwv, gravit, stebol, epsilo, tmelt, &
                                 latvap, latice, rh2o, zvir, cpvir, rhoh2o, pstd,  &
                                 karman, rhodair
   use MG,                 only:stratiform_init !! sxj 2008-11-10
   use aerosol_mass_interface ,only:aerosol_mass_init !! sxj-2009-03-09
   use prescribed_aerosols,    only:aerosol_initialize !! sxj-2009-03-09

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
# 40 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/inti.F90" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/comhyb.h" 1
!----------------------------------------------------------------------- 
! 
! Purpose: Hybrid level definitions: p = a*p0 + b*ps
!          interfaces   p(k) = hyai(k)*ps0 + hybi(k)*ps
!          midpoints    p(k) = hyam(k)*ps0 + hybm(k)*ps
! 
!-----------------------------------------------------------------------
!!!  vertical level definitions in LASG dynamical core: p = pes*sigma + pt
!!!        interfaces   ply(k) = ps*sig (k) + pmtop
!!!        midpoints    ply(k) = ps*sigl(k) + pmtop
!!!---------------------------------------------------------------------
!!!(wanhui 2003.04.30)
!!!(wanhui 2003.10.23)  (std.atm. variables removed)

      real(r8) hyai(plevp)       ! ps0 component of hybrid coordinate - interfaces
      real(r8) hybi(plevp)       ! ps component of hybrid coordinate - interfaces
      real(r8) hyam(plev)        ! ps0 component of hybrid coordinate - midpoints
      real(r8) hybm(plev)        ! ps component of hybrid coordinate - midpoints

!!    real(r8) hybd(plev)        ! difference  in b (hybi) across layers
      real(r8) hypi(plevp)       ! reference pressures at interfaces
      real(r8) hypm(plev)        ! reference pressures at midpoints
!!    real(r8) hypd(plev)        ! reference pressure layer thickness

      real(r8) ps0         ! base state sfc pressure for level definitions
!!    real(r8) psr         ! reference surface pressure for linearization
!!    real(r8) prsfac      ! log pressure extrapolation factor (time, space independent)

!!    integer nprlev       ! number of pure pressure levels at top

      real(r8) :: pmtop              !
      real(r8) :: sig (plevp)        !
      real(r8) :: sigl(plev)         !  fm2003 VPAR variables
      real(r8) :: dsig(plev)         !

!!(wanhui 2003.10.23)
!!------------------------------------------------------------
!!    real(r8) :: tbb (plevstd)         !
!!    real(r8) :: hbb (plevstd)         !
!!    real(r8) :: cbb (plevstd)         !
!!    real(r8) :: dcbb(plevstd)         !  fm2003 std. atm.
!!    real(r8) :: p00, t00              !
!!    real(r8) :: psb (plond,plat)      !
!!    real(r8) :: tsb (plond,plat)      !
!!------------------------------------------------------------
!!(2003.10.23)(these variables are in module stdatm now)


!!      common /comhyb/ hyai ,hyam  ,hybi ,hybm
!!      common /comhyb/ hybd ,hypi ,hypm  ,hypd
!!      common /comhyb/ ps0         ,psr         ,prsfac      ,nprlev

      common /comhyb/ hyai ,hybi ,hyam ,hybm
      common /comhyb/ hypi ,hypm
      common /comhyb/ ps0  ,pmtop
      common /comhyb/ sig  ,sigl , dsig
!!    common /comhyb/ tbb  ,hbb  , cbb  ,dcbb  ,p00 ,t00 ,psb ,tsb    !!(wh 2003.10.23)
 
# 41 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/inti.F90" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/RK_or_MG.h" 1
 
  character(len=2)  RK_or_MG
  parameter (RK_or_MG='MG')

# 42 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/inti.F90" 2

 ! the following code copy form cam3.5.8  
  ! !! Input/output arguments
   type(physics_state), pointer :: phys_state(:)
   ! local variables
   integer :: lchnk
   if (RK_or_MG=='MG') then
      allocate(phys_state(begchunk:endchunk))
       ! Set chunk id, number of columns, and coordinates
      do lchnk = begchunk,endchunk
         call physics_state_set_grid(lchnk, phys_state(lchnk))
      end do
   end if
!
!-----------------------------------------------------------------------
!
! Initialize physconst variables
! In adiabatic case, set zvir and cpvir explicitly to zero instead of 
! computing as (rh2o/rair - 1.) and (cpwv/cpair - 1.) respectively, in order 
! to guarantee an identical zero.
!
   if (adiabatic .or. ideal_phys) then
      rh2o  = rair
      zvir  = 0.
      cpwv  = cpair
      cpvir = 0.
   else
      rh2o  = shr_const_rwv
      zvir  = shr_const_zvir
      cpwv  = shr_const_cpwv
      cpvir = cpwv/cpair - 1.
   end if
!
! Call time independent initialization routines for parameterizations.
!
   if (trace_gas) call chem_initialize
   call gw_inti (cpair   ,cpwv    ,gravit  ,rair    ,hypi    )
   call vd_inti (cpair   ,cpwv    ,gravit  ,rair    ,zvir   , &
                 hypm    ,karman    )
   call tsinti  (tmelt   ,latvap  ,rair    ,stebol  ,latice  )
   call radini  (gravit  ,cpair   ,epsilo  ,stebol  ,pstd*10.0 )
   call esinti  (epsilo  ,latvap  ,latice  ,rh2o    ,cpair  , &
                 tmelt   )
   call mfinti  (rair    ,cpair   ,gravit  ,latvap  ,rhoh2o  )
   call zm_convi( tmelt, epsilo, latvap, cpair )
   if (convection_scheme == 'Zhang_Neale') call zm_convi_3(hypi)   !!sxj
   call cldinti ()
    !
    ! initialization routine for prognostic cloud water
    !
    if (RK_or_MG=='MG') then
        call aerosol_initialize(phys_state(begchunk:endchunk))  !! sxj 2009-03-09
        call stratiform_init                                    !! sxj 2008-11-10
        call aerosol_mass_init()                                !! sxj 2009-03-09
    else if (RK_or_MG=='RK') then
        call inimc(tmelt, rhodair/1000.0, gravit, rh2o)   
    end if

end subroutine inti
