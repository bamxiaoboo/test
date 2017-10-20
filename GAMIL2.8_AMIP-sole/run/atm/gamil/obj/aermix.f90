# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/aermix.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/aermix.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/aermix.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/aermix.F90" 2
subroutine aermix(lchnk, ncol, pint, sulfmix, aermmr, rh)
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Set global mean tropospheric aerosol
! 
! Method: 
! Specify aerosol mixing ratio and compute relative humidity for later
! adjustment of aerosol optical properties. Aerosol mass mixing ratio
! is specified so that the column visible aerosol optical depth is a
! specified global number (tauvis). This means that the actual mixing
! ratio depends on pressure thickness of the lowest three atmospheric
! layers near the surface.
!
! Optical properties and relative humidity parameterization are from:
!
! J.T. Kiehl and B.P. Briegleb  "The Relative Roles of Sulfate Aerosols
! and Greenhouse Gases in Climate Forcing"  Science  260  pp311-314
! 16 April 1993
!
! Visible (vis) here means 0.5-0.7 micro-meters
! Forward scattering fraction is taken as asymmetry parameter squared
! 
! Author: J.T. Kiehl
! 
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8
   use ppgrid
!-----------------------------------------------------------------------
   implicit none
!-----------------------------------------------------------------------

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/ptrrgrid.h" 1
!----------------------------------------------------------------------- 
! 
! Purpose: Define radiation vertical grid
! 
! Author: CCM Core Group
! 
!-----------------------------------------------------------------------

      integer pverr    ! Number of vertical levels
      integer pverrp   ! pverr + 1
!
      parameter(pverr = 26)
      parameter(pverrp = pverr + 1)
 
# 35 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/aermix.F90" 2
!------------------------------Commons----------------------------------

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
 
# 37 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/aermix.F90" 2
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

# 39 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/aermix.F90" 2
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: lchnk                 ! chunk identifier
   integer, intent(in) :: ncol                  ! number of atmospheric columns

   real(r8), intent(in) :: pint(pcols,pverrp)   ! Rad level interface press. (dynes/cm2)
   real(r8), intent(in) :: sulfmix(pcols,pver)  ! time interpolated sulfate mass mixing ratio
!
! Output arguments
!
   real(r8), intent(out) :: aermmr(pcols,pverr)  ! Rad level aerosol mass mixing ratio
   real(r8), intent(out) :: rh(pcols,pverr)      ! Rad level relative humidity (fraction)

!
!---------------------------Local variables-----------------------------
!
   integer i          ! Longitude index
   integer k          ! Level index
!
   real(r8) kaervs    ! Visible extinction coefficiant of aerosol (m2/g)
   real(r8) omgvis    ! Visible single scattering albedo
   real(r8) gvis      ! Visible scattering asymmetry parameter
   real(r8) rhcnst    ! Constant relative humidity factor
!
! Relative humidity factor
!
   real(r8) rhfac     ! Multiplication factor for kaer
   real(r8) rhpc      ! Level relative humidity in %
   real(r8) a0        ! Constant in relative humidity mult factor
   real(r8) a1        ! Constant in relative humidity mult factor
   real(r8) a2        ! Constant in relative humidity mult factor
   real(r8) a3        ! Constant in relative humidity mult factor
!
!--------------------------Data Statements------------------------------
!
   data a0 / -9.2906106183    /
   data a1 /  0.52570211505   /
   data a2 / -0.0089285760691 /
   data a3 /  5.0877212432e-05/
!
   data kaervs / 5.3012   /
   data omgvis / 0.999999 /
   data gvis   / 0.694889 /
   data rhcnst /  .80     /
!
!-----------------------------------------------------------------------
!
! Set relative humidity and factor; then aerosol amount.
!
   do i=1,ncol
      do k=1,pverr
!
         rh(i,k) = rhcnst
!
! Compute relative humidity factor for the extinction coefficiant; this
! factor accounts for the dependence of size distribution on relative
! humidity:
!
         if ( rh(i,k) > .90 ) then
            rhfac = 2.8
         else if (rh(i,k) < .60 ) then
            rhfac = 1.0
         else
            rhpc  = 100. * rh(i,k)
            rhfac = (a0 + a1*rhpc + a2*rhpc**2 + a3*rhpc**3)
         endif
!
! Compute aerosol mass mixing ratio for specified levels (1.e4 factor is
! for units conversion of the extinction coefficiant from m2/g to cm2/g)
!
         if ( k >= pverrp-mxaerl ) then
            aermmr(i,k) = gravit*tauvis / &
                          (1.e4*kaervs*rhfac*(1.-omgvis*gvis*gvis) * &
                          (pint(i,pverrp)-pint(i,pverrp-mxaerl))) + &
                          sulfmix(i,k)
         else
            aermmr(i,k) = sulfmix(i,k)
         endif
!
      enddo
   enddo
!
   return
end subroutine aermix

