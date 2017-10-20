# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/advection/slt/plevs0.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/advection/slt/plevs0.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/advection/slt/plevs0.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/advection/slt/plevs0.F90" 2

subroutine plevs0 (ncol    , ncold   ,nver    ,ps      ,pint    , &
                   pmid    ,pdel)


!! (wanhui 2003.05.17)
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Define the pressures of the interfaces and midpoints from the
! coordinate definitions and the surface pressure.
! 
! Method: 
! 
! Author: B. Boville
! 
!-----------------------------------------------------------------------
!
! $Id: plevs0.F90,v 1.1.2.1 2002/06/15 13:46:59 erik Exp $
! $Author: erik $
!
!-----------------------------------------------------------------------

  use shr_kind_mod, only: r8 => shr_kind_r8
  use pmgrid
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
 
# 30 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/advection/slt/plevs0.F90" 2

!-----------------------------------------------------------------------
  integer , intent(in)  :: ncol               ! Longitude dimension
  integer , intent(in)  :: ncold              ! Declared longitude dimension
  integer , intent(in)  :: nver               ! vertical dimension
  real(r8), intent(in)  :: ps(ncold)          ! Surface pressure (pascals)
  real(r8), intent(out) :: pint(ncold,nver+1) ! Pressure at model interfaces
  real(r8), intent(out) :: pmid(ncold,nver)   ! Pressure at model levels
  real(r8), intent(out) :: pdel(ncold,nver)   ! Layer thickness (pint(k+1) - pint(k))
!-----------------------------------------------------------------------

!---------------------------Local workspace-----------------------------
  integer i,k             ! Longitude, level indices
!-----------------------------------------------------------------------
!
! Set interface pressures
!
  do k=1,nver+1
     do i=1,ncol
!!      pint(i,k) = hyai(k)*ps0 + hybi(k)*ps(i)
        pint(i,k) = sig(k)*( ps(i)-pmtop*100.0 ) + pmtop*100.0
     end do
  end do
!
! Set midpoint pressures and layer thicknesses
!
  do k=1,nver
     do i=1,ncol
!!      pmid(i,k) = hyam(k)*ps0 + hybm(k)*ps(i)
        pmid(i,k) = sigl(k)*( ps(i)-pmtop*100.0 ) + pmtop*100.0
        pdel(i,k) = pint(i,k+1) - pint(i,k)
     end do
  end do

  return
end subroutine plevs0

