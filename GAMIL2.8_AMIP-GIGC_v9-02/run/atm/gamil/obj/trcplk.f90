# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/trcplk.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/trcplk.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/trcplk.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/trcplk.F90" 2
subroutine trcplk(lchnk   ,ncol    ,                            &
                  tint    ,tlayr   ,tplnke  ,emplnk  ,abplnk1 , &
                  abplnk2 )
!----------------------------------------------------------------------- 
! 
! Purpose: 
!   Calculate Planck factors for absorptivity and emissivity of
!   CH4, N2O, CFC11 and CFC12
! 
! Method: 
!   Planck function and derivative evaluated at the band center.
! 
! Author: J. Kiehl
! 
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8
   use ppgrid

   implicit none
!------------------------------Arguments--------------------------------

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
 
# 24 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/trcplk.F90" 2
!
! Input arguments
!
   integer, intent(in) :: lchnk                ! chunk identifier
   integer, intent(in) :: ncol                 ! number of atmospheric columns

   real(r8), intent(in) :: tint(pcols,pverp)   ! interface temperatures
   real(r8), intent(in) :: tlayr(pcols,pverp)  ! k-1 level temperatures
   real(r8), intent(in) :: tplnke(pcols)       ! Top Layer temperature
!
! output arguments
!
   real(r8), intent(out) :: emplnk(14,pcols)         ! emissivity Planck factor
   real(r8), intent(out) :: abplnk1(14,pcols,pverp)  ! non-nearest layer Plack factor
   real(r8), intent(out) :: abplnk2(14,pcols,pverp)  ! nearest layer factor

!
!--------------------------Local Variables------------------------------
!
   integer wvl                   ! wavelength index
   integer i,k                   ! loop counters
!
   real(r8) f1(14)                   ! Planck function factor
   real(r8) f2(14)                   !        "
   real(r8) f3(14)                   !        "
!
!--------------------------Data Statements------------------------------
!
   data f1 /5.85713e8,7.94950e8,1.47009e9,1.40031e9,1.34853e8, &
            1.05158e9,3.35370e8,3.99601e8,5.35994e8,8.42955e8, &
            4.63682e8,5.18944e8,8.83202e8,1.03279e9/
   data f2 /2.02493e11,3.04286e11,6.90698e11,6.47333e11, &
            2.85744e10,4.41862e11,9.62780e10,1.21618e11, &
            1.79905e11,3.29029e11,1.48294e11,1.72315e11, &
            3.50140e11,4.31364e11/
   data f3 /1383.0,1531.0,1879.0,1849.0,848.0,1681.0, &
            1148.0,1217.0,1343.0,1561.0,1279.0,1328.0, &
            1586.0,1671.0/
!
!-----------------------------------------------------------------------
!
! Calculate emissivity Planck factor
!
   do wvl = 1,14
      do i = 1,ncol
         emplnk(wvl,i) = f1(wvl)/(tplnke(i)**4.0*(exp(f3(wvl)/tplnke(i))-1.0))
      end do
   end do
!
! Calculate absorptivity Planck factor for tint and tlayr temperatures
!
   do wvl = 1,14
      do k = ntoplw, pverp
         do i = 1, ncol
!
! non-nearlest layer function
!
            abplnk1(wvl,i,k) = (f2(wvl)*exp(f3(wvl)/tint(i,k)))  &
                               /(tint(i,k)**5.0*(exp(f3(wvl)/tint(i,k))-1.0)**2.0)
!
! nearest layer function
!
            abplnk2(wvl,i,k) = (f2(wvl)*exp(f3(wvl)/tlayr(i,k))) &
                               /(tlayr(i,k)**5.0*(exp(f3(wvl)/tlayr(i,k))-1.0)**2.0)
         end do
      end do
   end do
!
   return
end subroutine trcplk

