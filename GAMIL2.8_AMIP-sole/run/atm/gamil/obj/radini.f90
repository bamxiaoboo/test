# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/radini.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/radini.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/radini.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/radini.F90" 2
subroutine radini(gravx   ,cpairx  ,epsilox ,stebolx, pstdx )
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Initialize various constants for radiation scheme; note that
! the radiation scheme uses cgs units.
! 
! Method: 
! <Describe the algorithm(s) used in the routine.> 
! <Also include any applicable external references.> 
! 
! Author: W. Collins (H2O parameterization) and J. Kiehl
! 
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8
   use ppgrid,       only: pver, pverp
   use comozp,       only: cplos, cplol
   use pmgrid,       only: masterproc, plev, plevp, plevstd,plond,plat
   use radae,        only: radaeini
   use physconst,    only: mwdry, mwco2
   use constituents, only: co2vmr

    use mpishorthand

   implicit none


# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/crdcon.h" 1
!
! $Id: crdcon.h,v 1.6 2001/10/03 21:46:17 erik Exp $
! $Author: erik $
!
!
! Radiation constants
!
      common/crdcon/gravit  ,rga     ,cpair   ,epsilo  ,sslp
      common/crdcon/stebol  ,rgsslp  ,co2mmr  ,dpfo3   ,dpfco2
      common/crdcon/dayspy  ,pie     ,mxaerl
      common/crdcon/ntoplw
!
      real(r8) gravit     ! Acceleration of gravity
      real(r8) rga        ! 1./gravit
      real(r8) cpair      ! Specific heat of dry air
      real(r8) epsilo     ! Ratio of mol. wght of H2O to dry air
      real(r8) sslp       ! Standard sea-level pressure
      real(r8) stebol     ! Stefan-Boltzmann's constant
      real(r8) rgsslp     ! 0.5/(gravit*sslp)
      real(r8) co2mmr     ! CO2 mass mixing ratio
      real(r8) dpfo3      ! Voigt correction factor for O3
      real(r8) dpfco2     ! Voigt correction factor for CO2
      real(r8) dayspy     ! Number of days per 1 year
      real(r8) pie        ! 3.14.....
!
      integer mxaerl  ! Number of levels from bottom for bckgrnd aerosol
      integer ntoplw      ! top level to solve for longwave cooling
!
 
# 30 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/radini.F90" 2

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
 
# 31 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/radini.F90" 2

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
# 32 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/radini.F90" 2
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   real(r8), intent(in) :: gravx      ! Acceleration of gravity (MKS)
   real(r8), intent(in) :: cpairx     ! Specific heat of dry air (MKS)
   real(r8), intent(in) :: epsilox    ! Ratio of mol. wght of H2O to dry air
   real(r8), intent(in) :: stebolx    ! Stefan-Boltzmann's constant (MKS)
   real(r8), intent(in) :: pstdx      ! Standard pressure (Pascals)
!
!---------------------------Local variables-----------------------------

   real(r8), parameter :: rmwco2 = mwco2/mwdry   ! ratio of molecular weights of co2 to dry air
!
   integer k       ! Loop variable

   real(r8) v0         ! Volume of a gas at stp (m**3/kmol)
   real(r8) p0         ! Standard pressure (pascals)
   real(r8) amd        ! Effective molecular weight of dry air (kg/kmol)
   real(r8) goz        ! Acceleration of gravity (m/s**2)
!
!-----------------------------------------------------------------------
!
! Set general radiation consts; convert to cgs units where appropriate:
!
   gravit  =  100.*gravx
   rga     =  1./gravit
   cpair   =  1.e4*cpairx
   epsilo  =  epsilox
   sslp    =  1.013250e6
   stebol  =  1.e3*stebolx
   rgsslp  =  0.5/(gravit*sslp)
   dpfo3   =  2.5e-3
   dpfco2  =  5.0e-3
   dayspy  =  365.
   pie     =  4.*atan(1.)
!
! Determine co2 global mass mixing ratio - only if not ramping co2vmr
! Note: when ramping co2vmr, co2mmr is computed in subroutine ramp_ghg
!
   if (.not. doRamp_ghg) then
      co2mmr = rmwco2 * co2vmr
   endif
!
! Initialize ozone data.
!
   v0  = 22.4136         ! Volume of a gas at stp (m**3/kmol)
   p0  = 0.1*sslp        ! Standard pressure (pascals)
   amd = 28.9644         ! Molecular weight of dry air (kg/kmol)
   goz = gravx           ! Acceleration of gravity (m/s**2)
!
! Constants for ozone path integrals (multiplication by 100 for unit
! conversion to cgs from mks):
!
   cplos = v0/(amd*goz)       *100.0
   cplol = v0/(amd*goz*p0)*0.5*100.0
!
! Derived constants
! mxaerl = max number of levels (from bottom) for background aerosol
! Limit background aerosol height to regions below 900 mb
!
   mxaerl = 0
   do k=pver,1,-1
      if (hypm(k) >= 9.e4) mxaerl = mxaerl + 1
   end do
   mxaerl = max(mxaerl,1)
   if (masterproc) then
      write(6,*)'RADINI:  Background aerosol will be limited to ', &
                'bottom ',mxaerl,' model interfaces. Top interface is ', &
                hypi(pverp-mxaerl),' pascals'
   end if

! If the top model level is above ~90 km (0.1 Pa), set the top level to compute
! longwave cooling to about 80 km (1 Pa)
   if (hypm(1) .lt. 0.1) then
      do k = 1, pver
         if (hypm(k) .lt. 1.) ntoplw  = k
      end do
   else
      ntoplw = 1
   end if
   if (masterproc) then
      write (6,*) 'RADINI: ntoplw =',ntoplw, ' pressure:',hypm(ntoplw)
   endif

   call radaeini( pstdx, mwdry, mwco2 )
   return
end subroutine radini
