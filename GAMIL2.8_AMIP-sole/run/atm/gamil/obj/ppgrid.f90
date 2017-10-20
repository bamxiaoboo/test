# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/ppgrid.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/ppgrid.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/ppgrid.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/ppgrid.F90" 2

module ppgrid

!----------------------------------------------------------------------- 
! 
! Purpose: 
! Initialize physics grid resolution parameters
!  for a chunked data structure
! 
! Author: 
! 
!-----------------------------------------------------------------------

! Grid point resolution parameters

   integer pcols      ! number of columns (max)
   integer pver       ! number of vertical levels
   integer pvermx     ! number of subsurface levels
   integer pverp      ! pver + 1

   parameter (pcols  = 16)
   parameter (pver   = 26)
   parameter (pvermx = 4)
   parameter (pverp  = pver + 1  )
!
! start, end indices for chunks owned by a given MPI task
! (set in phys_grid_init).
!
   integer :: begchunk = 0            ! 
   integer :: endchunk = -1           ! 

end module ppgrid
