# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/ioUnitMod.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/ioUnitMod.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/ioUnitMod.F90" 2

# 1 "./preproc.h" 1






 
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/ioUnitMod.F90" 2

module ioUnitMod

  use shr_kind_mod, only: r8 => shr_kind_r8
  implicit none

  logical :: lsmiou(99)  !I/O file unit numbers (1 to 99): true if active

!=======================================================================
contains
!=======================================================================

  integer function getavu()

!----------------------------------------------------------------------- 
! 
! Purpose: 
! get next available Fortran unit number
!
! Method: 
! Get next available Fortran unit number itst. Set lsmiou(itst), in 
! lsmio common block, true. If coupled to CAM, use CAM function navu
! to get available unit number, in which case lsmiou is not needed.
! 
! Author: Gordon Bonan
! 
!-----------------------------------------------------------------------


    use units     !CAM units module


! ------------------------ local variables ------------------------
    integer itst  !Fortran unit number
! -----------------------------------------------------------------


    getavu = getunit()
    RETURN
# 53 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/ioUnitMod.F90"
  end function getavu

!=======================================================================

  subroutine relavu (iunit)

!----------------------------------------------------------------------- 
! 
! Purpose: 
! close and release Fortran unit no longer in use
!
! Method: 
! Close and release Fortran unit number iunit. Set lsmiou(iunit) to 
! false. If coupled to cam, use cam function relunit to close/release 
! unit number.
! 
! Author: Gordon Bonan
! 
!-----------------------------------------------------------------------


    use units     !CAM units module


! ------------------------ arguments ------------------------------
    integer, intent(in) :: iunit    !Fortran unit number
! -----------------------------------------------------------------


    close(iunit)
    call freeunit(iunit)
# 96 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/ioUnitMod.F90"
    return
  end subroutine relavu

!=======================================================================

end module ioUnitMod







