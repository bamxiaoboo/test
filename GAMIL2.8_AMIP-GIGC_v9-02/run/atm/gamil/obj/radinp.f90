# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/radinp.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/radinp.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/radinp.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/radinp.F90" 2
subroutine radinp(lchnk   ,ncol    ,                            &
                  pmid    ,pint    ,o3vmr   , pmidrd  ,&
                  pintrd  ,eccf    ,o3mmr   )
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Set latitude and time dependent arrays for input to solar
! and longwave radiation.
! Convert model pressures to cgs, and compute ozone mixing ratio, needed for
! the solar radiation.
! 
! Method: 
! <Describe the algorithm(s) used in the routine.> 
! <Also include any applicable external references.> 
! 
! Author: CCM1, CMS Contact J. Kiehl
! 
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8
   use ppgrid
   use shr_orb_mod
   use time_manager, only: get_curr_calday

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
 
# 29 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/radinp.F90" 2

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

# 30 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/radinp.F90" 2
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: lchnk                ! chunk identifier
   integer, intent(in) :: ncol                 ! number of atmospheric columns

   real(r8), intent(in) :: pmid(pcols,pver)    ! Pressure at model mid-levels (pascals)
   real(r8), intent(in) :: pint(pcols,pverp)   ! Pressure at model interfaces (pascals)
   real(r8), intent(in) :: o3vmr(pcols,pver)   ! ozone volume mixing ratio
!
! Output arguments
!
   real(r8), intent(out) :: pmidrd(pcols,pver)  ! Pressure at mid-levels (dynes/cm*2)
   real(r8), intent(out) :: pintrd(pcols,pverp) ! Pressure at interfaces (dynes/cm*2)
   real(r8), intent(out) :: eccf                ! Earth-sun distance factor
   real(r8), intent(out) :: o3mmr(pcols,pver)   ! Ozone mass mixing ratio

!
!---------------------------Local variables-----------------------------
!
   integer i                ! Longitude loop index
   integer k                ! Vertical loop index

   real(r8) :: calday           ! current calendar day
   real(r8) amd                 ! Effective molecular weight of dry air (g/mol)
   real(r8) amo                 ! Molecular weight of ozone (g/mol)
   real(r8) vmmr                ! Ozone volume mixing ratio
   real(r8) delta               ! Solar declination angle

   save     amd   ,amo

   data amd   /  28.9644   /
   data amo   /  48.0000   /
!
!-----------------------------------------------------------------------
!
   calday = get_curr_calday()
   call shr_orb_decl (calday  ,eccen     ,mvelpp  ,lambm0  ,obliqr  , &
                      delta   ,eccf)

!
! Convert pressure from pascals to dynes/cm2
!
   do k=1,pver
      do i=1,ncol
         pmidrd(i,k) = pmid(i,k)*10.0
         pintrd(i,k) = pint(i,k)*10.0
      end do
   end do
   do i=1,ncol
      pintrd(i,pverp) = pint(i,pverp)*10.0
   end do
!
! Convert ozone volume mixing ratio to mass mixing ratio:
!
   vmmr = amo/amd
   do k=1,pver
      do i=1,ncol
         o3mmr(i,k) = vmmr*o3vmr(i,k)
      end do
   end do
!
   return
end subroutine radinp
