# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/tsinti.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/tsinti.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/tsinti.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/tsinti.F90" 2

subroutine tsinti (tmeltx, latvapx, rairx, stebolx, laticex)
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Initialize surface temperature calculation constants
! 
! Method: 
! <Describe the algorithm(s) used in the routine.> 
! <Also include any applicable external references.> 
! 
! Author: L. Buja
! 
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8

   implicit none


# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/comtsc.h" 1
!
! $Id: comtsc.h,v 1.4 2000/06/02 16:20:40 jet Exp $
! $Author: jet $
!
!
! Constants for surface temperature/energy exchange calculations
!
      common/comtsc/latice  ,tmelt   ,latvap  ,rair    ,stebol  
      common/comtsc/snwedp  
!
      real(r8) latice     ! Latent heat of fusion
      real(r8) tmelt      ! Melting temperature of snow and ice
      real(r8) latvap     ! Latent heat of vaporization
      real(r8) rair       ! Gas constant for dry air
      real(r8) stebol     ! Stefan-Boltzmann constant
      real(r8) snwedp     ! Snow equivalent depth factor 
!
 
# 22 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/tsinti.F90" 2
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   real(r8) tmeltx         ! Melting temperature of snow and ice
   real(r8) latvapx        ! Latent heat of vaporization
   real(r8) rairx          ! Gas constant for dry air
   real(r8) stebolx        ! Stefan-Boltzmann constant
   real(r8) laticex        ! latent heat of fusion
!
!-----------------------------------------------------------------------
!
   latice = laticex    ! Latent heat of fusion at 0'C = 3.336e5 J/Kg
   tmelt  = tmeltx
   latvap = latvapx
   rair   = rairx
   stebol = stebolx
   snwedp = 10.0       ! 10:1 Snow:water equivalent depth factor
!
   return
end subroutine tsinti

