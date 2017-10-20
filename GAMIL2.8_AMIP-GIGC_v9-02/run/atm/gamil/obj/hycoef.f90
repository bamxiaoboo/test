# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/hycoef.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/hycoef.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/hycoef.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/hycoef.F90" 2

subroutine hycoef 

!! (wanhui 2003.05.16)
!! (wanhui 2003.10.23)
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Defines the locations of model interfaces from input data in the
! hybrid coordinate scheme.  Actual pressure values of model level
! interfaces are determined elsewhere from the fields set here.
! 
! Method: 
! the following fields are set:
! hyai     fraction of reference pressure used for interface pressures
! hyam     fraction of reference pressure used for midpoint pressures
! hybi     fraction of surface pressure used for interface pressures
! hybm     fraction of surface pressure used for midpoint pressures
! hybd     difference of hybi's
! hypi     reference state interface pressures
! hypm     reference state midpoint pressures
! hypd     reference state layer thicknesses
! hypdln   reference state layer thicknesses (log p)
! hyalph   distance from interface to level (used in integrals)
! prsfac   log pressure extrapolation factor (used to compute psl)
! 
! Author: B. Boville
! 
!-----------------------------------------------------------------------
!
! $Id: hycoef.F90,v 1.2.4.1 2002/06/15 13:47:27 erik Exp $
! $Author: erik $
!
!-----------------------------------------------------------------------

  use shr_kind_mod, only: r8 => shr_kind_r8
  use pmgrid
  use stdatm, only: p00   !!(wh 2003.10.23)

  implicit none


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
 
# 45 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/hycoef.F90" 2

!---------------------------Local workspace-----------------------------
    integer k                 ! Level index
!!  real(r8) amean,bmean,atest,btest,eps
!-----------------------------------------------------------------------
!
!!  eps    = 1.e-05
!!  nprlev = 0
    ps0    = 1.0e5            ! Base state surface pressure (pascals)
!!  psr    = 1.0e5            ! Reference surface pressure (pascals)
!
! Set layer locations
!
! Reference state interface pressures
!
  do k=1,plevp
     hypi(k) = hyai(k)*ps0 + hybi(k)*p00
  end do
!
  do k=1,plev
     hypm(k) = 0.5*( hypi(k) + hypi(k+1) ) 
  end do

  return
end subroutine hycoef

