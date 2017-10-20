# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/spmd_phys.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/spmd_phys.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/spmd_phys.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/spmd_phys.F90" 2

module spmd_phys

!----------------------------------------------------------------------- 
! 
! Purpose:  implementation of CAM for physics.
! 
! Author: CCM Core Group
! 
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8
   use pmgrid, only: beglat, endlat
   use ppgrid, only: begchunk, endchunk
   implicit none

   private
   public spmdinit_phys

CONTAINS

!========================================================================

   subroutine spmdinit_phys ()
      begchunk = beglat
      endchunk = endlat

      return
   end subroutine spmdinit_phys

end module spmd_phys
