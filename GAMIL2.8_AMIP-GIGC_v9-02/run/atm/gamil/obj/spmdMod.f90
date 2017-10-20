# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/spmdMod.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/spmdMod.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/spmdMod.F90" 2

# 1 "./preproc.h" 1






 
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/spmdMod.F90" 2

module spmdMod

!-----------------------------------------------------------------------
!
! Purpose:
! MPI routines for initialization and computing arguments for
! gatherv and scatterv operations
!
! Method:
!
! Author: Mariana Vertenstein
!
!-----------------------------------------------------------------------
! $Id: spmdMod.F90,v 1.7.2.2 2001/11/26 15:28:11 mvertens Exp $
!-----------------------------------------------------------------------













  use mpishorthand
  use spmd_dyn, only: npes
  use pmgrid  , only: masterproc, iam
















  integer, public, allocatable :: proc_landi(:)
  integer, public, allocatable :: proc_landf(:)
  integer, public, allocatable :: proc_patchi(:)
  integer, public, allocatable :: proc_patchf(:)
  integer, public, allocatable :: proc_patchpts(:)
  integer, public, allocatable :: proc_landpts(:)

  SAVE

!===============================================================================
CONTAINS
!===============================================================================

# 157 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/spmdMod.F90"

!===============================================================================

  subroutine spmd_init_patch

!-----------------------------------------------------------------------
!
! Purpose: Initialize arrays for number of land/patch points per proc
!
!-----------------------------------------------------------------------

    allocate (proc_landi(0:npes-1))
    allocate (proc_landf(0:npes-1))
    allocate (proc_landpts(0:npes-1))
    allocate (proc_patchi(0:npes-1))
    allocate (proc_patchf(0:npes-1))
    allocate (proc_patchpts(0:npes-1))

    return
  end subroutine spmd_init_patch

!===============================================================================

  subroutine compute_mpigs_patch (nfact, numtot, numperproc, displs)

!------------------------------------------------------------------
!
! Purpose: Compute arguments for gatherv, scatterv for patche vectors
!
!------------------------------------------------------------------

    implicit none

! ------------------- arguments -----------------------------------
    integer, intent(in ) :: nfact                ! multiplicative factor for patches
    integer, intent(out) :: numtot               ! total number of elements (to send or recv)
    integer, intent(out) :: numperproc(0:npes-1) ! per-PE number of items to receive
    integer, intent(out) :: displs(0:npes-1)     ! per-PE displacements
!------------------------------------------------------------------

! ---------------------- local variables --------------------------
    integer :: p                                 ! index
!------------------------------------------------------------------

    numtot = (proc_patchpts(iam))*nfact

    do p=0,npes-1
       numperproc(p) = proc_patchpts(p)*nfact
    end do

    displs(0) = 0
    do p=1,npes-1
       displs(p) = displs(p-1) + numperproc(p-1)
    end do

  end subroutine compute_mpigs_patch

!===============================================================================

  subroutine compute_mpigs_land (nfact, numtot, numperproc, displs)

!------------------------------------------------------------------
!
! Purpose: Compute arguments for gatherv, scatterv for land vectors
!
!------------------------------------------------------------------

    implicit none

! ------------------- arguments -----------------------------------
    integer, intent(in ) :: nfact                ! multiplicative factor for patches
    integer, intent(out) :: numtot               ! total number of elements (to send or recv)
    integer, intent(out) :: numperproc(0:npes-1) ! per-PE number of items to receive
    integer, intent(out) :: displs(0:npes-1)     ! per-PE displacements
!------------------------------------------------------------------

! ---------------------- local variables --------------------------
    integer :: p                                 ! index
!------------------------------------------------------------------

    numtot = (proc_landpts(iam))*nfact

    do p=0,npes-1
       numperproc(p) = proc_landpts(p)*nfact
    end do

    displs(0) = 0
    do p=1,npes-1
       displs(p) = displs(p-1) + numperproc(p-1)
    end do

  end subroutine compute_mpigs_land

!===============================================================================



end module spmdMod


