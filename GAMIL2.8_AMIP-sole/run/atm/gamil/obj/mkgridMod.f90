# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/mksrfdata/mkgridMod.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/mksrfdata/mkgridMod.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/mksrfdata/mkgridMod.F90" 2

# 1 "./preproc.h" 1






 
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/mksrfdata/mkgridMod.F90" 2

module mkgridMod

!----------------------------------------------------------------------- 
! 
! Purpose: 
! Routines to create land model grid
! 
! Method: 
!
! Author: Mariana Vertenstein
! 
!-----------------------------------------------------------------------
! $Id: mkgridMod.F90,v 1.9.2.3 2002/06/15 13:50:39 erik Exp $
!-----------------------------------------------------------------------


  use shr_kind_mod, only: r8 => shr_kind_r8
  use clm_varpar          !parameters 
  use clm_varsur          !surface variables 
  use clm_varctl          !run control variables
  use fileutils, only : getfil
  use areaMod             !area averaging routines
  use spmdMod, only: masterproc
  implicit none

!=======================================================================
contains
!=======================================================================

# 532 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/mksrfdata/mkgridMod.F90"

!=======================================================================



  subroutine mkgrid_cam(cam_longxy, cam_latixy, cam_numlon, cam_landfrac, cam_landmask)

!----------------------------------------------------------------------- 
! 
! Purpose: 
! Generate land model grid when mode is CAM or CSM
! For CAM mode get grid AND fractional land and land mask from cam model
! For CSM mode get grid AND fractional land and land mask from coupler
! 
! Method: 
! 
! Author: Mariana Vertenstein
! 
!-----------------------------------------------------------------------

! ------------------------ arguments -----------------------------------
    real(r8), intent(in) :: cam_longxy(:,:)      !cam longitudes 
    integer , intent(in) :: cam_numlon(:)        !number of cam longitudes per latitude
    real(r8), intent(in) :: cam_latixy(:,:)      !cam latitudes  
    real(r8), intent(in) :: cam_landfrac(:,:)    !cam land fraction
    integer , intent(in) :: cam_landmask(:,:)    !cam land mask
! -----------------------------------------------------------------

! ------------------------ local variables ------------------------
    integer i,j     ! loop indices
! -----------------------------------------------------------------

! Determine if grid has pole points - if so, make sure that north pole
! is non-land and south pole is land

    if (abs((cam_latixy(1,lsmlat) - 90.)) < 1.e-6) then
       pole_points = .true.
       if ( masterproc ) write(6,*)'MKGRIDMOD: model has pole_points' 
       do i = 1,cam_numlon(1)
          if (cam_landmask(i,1) /= 1) then
             write(6,*)'cam grid with pole points has non-land at south pole'
             write(6,*)'longitude index= ',i
             call endrun
          endif
       end do
       do i = 1,cam_numlon(lsmlat)
          if (cam_landmask(i,lsmlat) == 1) then
             write(6,*)'cam grid with pole points has land at north pole'
             write(6,*)'longitude index= ',i
             call endrun
          endif
       end do
    else
       pole_points = .false.
       if ( masterproc ) write(6,*)'MKGRIDMOD: model does not have pole_points' 
    endif
          
! Determine land grid, land mask and land fraction

    numlon(:) = cam_numlon(:)  

    fullgrid = .true.
    do j = 1,lsmlat
       if (cam_numlon(j) < lsmlon) fullgrid = .false.
    end do

    do j = 1,lsmlat
       do i = 1,numlon(j)
          longxy(i,j)   = cam_longxy(i,j) 
          latixy(i,j)   = cam_latixy(i,j)
          landmask(i,j) = cam_landmask(i,j)
          landfrac(i,j) = cam_landfrac(i,j)
       end do
    end do

! Define land grid edges and grid cell areas

    call celledge (lsmlat, lsmlon, numlon, longxy, latixy, &
                   lats  , lonw  )

    call cellarea (lsmlat, lsmlon, numlon, lats, lonw, &
                   area   )

! Determine land fraction and land mask

    return
  end subroutine mkgrid_cam



end module mkgridMod
