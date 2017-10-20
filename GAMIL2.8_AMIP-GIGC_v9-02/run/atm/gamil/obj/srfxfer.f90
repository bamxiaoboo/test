# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/srfxfer.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/srfxfer.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/srfxfer.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/srfxfer.F90" 2
subroutine srfxfer(lchnk   ,ncol    ,psm1tmp ,um1     ,vm1     ,tm1     , &
                   qm1     ,exner   ,zm      ,pmidm1  ,rpdel   )
!-----------------------------------------------------------------------
!
! Purpose:
! Transfer atmospheric fields into common block /comsrf/
!
! Method:
! <Describe the algorithm(s) used in the routine.>
! <Also include any applicable external references.>
!
! Author: L. Bath  CMS Contact: M. Vertenstein
!
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8
   use ppgrid
   use comsrf



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
 
# 26 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/srfxfer.F90" 2
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: lchnk                 ! Chunk index
   integer, intent(in) :: ncol
!
   real(r8), intent(in) :: um1(pcols)           ! Bottom level u wind
   real(r8), intent(in) :: vm1(pcols)           ! Bottom level v wind
   real(r8), intent(in) :: tm1(pcols)           ! Bottom level temperature
   real(r8), intent(in) :: qm1(pcols)           ! Bottom level specific humidity
   real(r8), intent(in) :: exner(pcols)         ! Bottom level Exner function
   real(r8), intent(in) :: zm(pcols)            ! Bottom level height above surface
!
   real(r8), intent(in) :: psm1tmp(pcols)       ! Surface pressure
   real(r8), intent(in) :: pmidm1(pcols,pver)   ! Level pressures
   real(r8), intent(in) :: rpdel(pcols)         ! 1./(pint(k+1)-pint(k))

!
!---------------------------Local variables-----------------------------
!
   integer i                 ! Longitude index
!
!-----------------------------------------------------------------------
!
! Stuff global fluxes and state variables into common
!
   do i=1,ncol
      surface_state2d(lchnk)%tbot(i) = tm1(i)
      surface_state2d(lchnk)%thbot(i) = tm1(i) * exner(i)
      surface_state2d(lchnk)%zbot(i) = zm(i)
      surface_state2d(lchnk)%ubot(i) = um1(i)
      surface_state2d(lchnk)%vbot(i) = vm1(i)
      surface_state2d(lchnk)%qbot(i) = qm1(i)
      surface_state2d(lchnk)%pbot(i) = pmidm1(i,pver)
      psm1(i,lchnk) = psm1tmp(i)
      srfrpdel(i,lchnk) = rpdel(i)





   end do

   return
end subroutine srfxfer







