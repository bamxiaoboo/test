# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/preset.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/preset.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/preset.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/preset.F90" 2

subroutine preset

!! (wh 2003.04.28)
!! (wh 2004.04.14)
!-----------------------------------------------------------------------
!
! Purpose: Preset namelist ATMEXP input variables and initialize some other variables
!
! Method: Hardwire the values
!
! Author: CCM Core Group
!
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8
   use infnan,       only: inf
   use history,      only: fincl, fexcl, fhstpr, fwrtpr
   use pmgrid
   use tracers,      only: nusr_adv, nusr_nad
   use constituents, only: ch4vmr, n2ovmr, f11vmr, f12vmr, co2vmr
!! use pspect
   use rgrid
   use shr_orb_mod
   use dycore
   use comhd, only: dfs0          !! (wh 2004.04.14)

   use ice_dh, only: prognostic_icesnow,reset_csim_iceprops

   use time_manager, only: timemgr_preset
!-----------------------------------------------------------------------
   implicit none
!------------------------------Commons----------------------------------

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/comadj.h" 1
!
! $Id: comadj.h,v 1.6 2001/08/10 22:07:55 boville Exp $
! $Author: boville $
!
!
! Convective adjustment
!
      common/comadj/ nlvdry
!
      integer nlvdry        ! Number of levels to apply dry adjustment
!

 
# 36 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/preset.F90" 2
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
# 38 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/preset.F90" 2
!-----------------------------------------------------------------------
!!#include <comhd.h>
!-----------------------------------------------------------------------

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
# 42 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/preset.F90" 2
!-----------------------------------------------------------------------

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/comtfc.h" 1
!----------------------------------------------------------------------- 
! 
! Purpose: time filter coefficient
! 
!-----------------------------------------------------------------------
      common /comtfc/ eps

      real(r8) eps             ! Time filter coefficient
 
# 44 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/preset.F90" 2
!-----------------------------------------------------------------------

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/comsol.h" 1
!
!	Common's to do with solar radiation
!
!	$Id: comsol.h,v 1.3 2000/06/02 16:20:40 jet Exp $
!
! Visible optical depth
!
      real(r8) tauvis     ! Visible optical depth

      common /comvis/ tauvis
!
! Solar constant
!
      real(r8) scon       ! Solar constant

      common /comsol/ scon
!
! Earth's orbital characteristics
!	
      real(r8) eccen       ! Earth's eccentricity factor (unitless) (typically 0 to 0.1)
      real(r8) obliq       ! Earth's obliquity angle (degree's) (-90 to +90) (typically 22-26)
      real(r8) mvelp       ! Earth's moving vernal equinox at perhelion (degree's) (0 to 360.0)
      integer iyear_AD ! Year (AD) to simulate above earth's orbital parameters for
!
! Orbital information after processed by orbit_params
!
      real(r8) obliqr      ! Earth's obliquity in radians
      real(r8) lambm0      ! Mean longitude of perihelion at the 
!                          ! vernal equinox (radians)
      real(r8) mvelpp      ! Earth's moving vernal equinox longitude
!                          ! of perihelion plus pi (radians)
!
      common /comorb/ eccen   , obliq   , mvelp   , obliqr  
      common /comorb/ lambm0  , mvelpp  , iyear_AD

# 46 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/preset.F90" 2
!-----------------------------------------------------------------------

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/perturb.h" 1
!----------------------------------------------------------------------- 
! 
! Purpose: Perturbation information
! 
! Author: Jim Rosinski
! 
!-----------------------------------------------------------------------
      common /perturb/ pertlim
      real(r8) pertlim        ! Bound for rectangular distribution of perturbation
!
 
# 48 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/preset.F90" 2
!-----------------------------------------------------------------------
   include 'netcdf.inc'
!-----------------------------------------------------------------------
!
! Preset character history variables here because module initialization of character arrays
! does not work on all machines
!
   fincl(:,:)  = ' '
   fexcl(:,:)  = ' '
   fhstpr(:,:) = ' '
   fwrtpr(:,:) = ' '
!
! Flags
!
   nlend       = .false.       ! end of run flag
   nlres       = .false.       ! continuation run flag
   nlhst       = .false.       ! regen run or branch run flag
   lbrnch      = .false.       ! branch run flag
   adiabatic   = .false.       ! no physics
   ideal_phys  = .false.       ! "idealized" model configuration
   aqua_planet = .false.       ! global oceans/analytical SST's
   print_step_cost = .false.   ! print per timestep cost info
!
! Tracers
!
   trace_gas   = .false.          ! greenhouse gas code not implemented
   trace_test1 = .false.          ! test tracer 1 code not implemented
   trace_test2 = .false.          ! test tracer 2 code not implemented
   trace_test3 = .false.          ! test tracer 3 code not implemented
   readtrace   = .true.           ! initialize from initial conditions file
   nusr_adv    = 0                ! zero user advected tracers
   nusr_nad    = 0                ! zero user non-advected tracers
!
! Ice flags
!

   prognostic_icesnow = .true.    ! snow falls on ice by default but
                                  ! it is limited to 0.5 meter.
   reset_csim_iceprops= .false.   ! use initial condition info unless
                                  ! need to reset ice properties in csim

!
   trace_gas   = .false.          ! greenhouse gas code not implemented
!
! Default run type is initialization
!
   nsrest = 0
!
! Default value for writing restart files
!
   nrefrq = 1      ! normal run, dispose with full history file
!
! Cycling flags for input boundary data files
!
   sstcyc = .true.
   icecyc = .true.
   ozncyc = .true.
!
! Model time defaults
!
   call timemgr_preset()
!
! Frequency in iterations of absorptivity/emissivity calc (negative
! values in model hours)
!
   iradae = -12
!
! Frequency of annual cycle sst update
!
   itsst  =  1
!
! Default frequency of shortwave and longwave radiation computations:
! once per hour (negative value implies model hours)
!
   iradsw = -1
   iradlw = -1
!
! Numerical scheme default values
!
   eps    = 0.06
!
! The Courant limiter is only used in eul dycore.
!
!!   kmxhdc = 5
   nlvdry = 3
!
!! Horizontal diffusion is used in both eul and sld dycores, but for sld
!! the parameters are zero in the production version.
!! Horizontal diffusion is not used in the fv dycore.
!!
!!   if (dycore_is ('EUL')) then
!!
!!     Set dif2 and dif4 for T42 resolution otherwise require setting on namelist
!!
!!      dif2   = 2.5e5   ! Leave dif2 to this value
!!      if ( (plon == 128) .and. (plat == 64) )then
!!        dif4   = 1.e16
!!      else
!!        dif4   = inf
!!      end if
!!   else
!!      dif2   = 0.
!!      dif4   = 0.
!!   end if
!!
    if (dycore_is ('EUL')) dfs0 = 0.02           !!(wh 2004.04.14)
!
!! No divergence damping
!!
!!   divdampn = 0.
!
! Precipitation threshold for PRECCINT, PRECLINT, PRECCFRQ, and PRECLFRQ output fields
! (mm/hr)
!
   precc_thresh = 0.1
   precl_thresh = 0.05
!
! Initialize volume mixing ratios
!
   ch4vmr = 1.714e-6
   n2ovmr = 0.311e-6
   f11vmr = 0.280e-9
   f12vmr = 0.503e-9
   co2vmr = 3.550e-4
!
! Orbital parameters.
! NOTE: if iyear_AD is set to SHR_ORB_UNDEF_INT after namelist input
! then namelist values of obliq,eccen,and mvelp are used otherwise
! obliq,eccen and mvelp are calculated based on iyear_AD
!
   iyear_ad = shr_orb_undef_int
   obliq    = shr_orb_undef_real
   eccen    = shr_orb_undef_real
   mvelp    = shr_orb_undef_real
!
! Solar constant
!
   scon       = 1.367e6
!
! Visible optical depth
!
   tauvis = .14







!
! rgrid: set default to full grid
!
   nlon(:) = plon
!
! Unit numbers: set to invalid
!
   nsds     = -1
   nrg      = -1
   nrg2     = -1
   ncid_ini = -1
   ncid_oz  = -1
   ncid_sst = -1
   ncid_trc = -1
   luhrest  = -1
!
! /perturb/
!
  pertlim = 0.0

   return
end subroutine preset
