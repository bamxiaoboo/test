# 1 "/data3/work/yuxinzhu/test/GAMIL2.8_AMIP-sole/run/lib/c_coupler/src/individual_src_files/shr_mpi_mod.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/GAMIL2.8_AMIP-sole/run/lib/c_coupler/src/individual_src_files/shr_mpi_mod.F90"

Module shr_mpi_mod

!-------------------------------------------------------------------------------
! PURPOSE: general layer on MPI functions
!-------------------------------------------------------------------------------

   use shr_kind_mod

   implicit none
   private

! PUBLIC: Public interfaces

   public :: shr_mpi_chkerr
   public :: shr_mpi_send
   public :: shr_mpi_recv
   public :: shr_mpi_bcast
   public :: shr_mpi_sum
   public :: shr_mpi_min
   public :: shr_mpi_max
   public :: shr_mpi_commsize
   public :: shr_mpi_commrank
   public :: shr_mpi_initialized
   public :: shr_mpi_abort
   public :: shr_mpi_barrier
   public :: shr_mpi_init
   public :: shr_mpi_finalize

   interface shr_mpi_send ; module procedure &
     shr_mpi_sendi0, &
     shr_mpi_sendi1, &
     shr_mpi_sendr0, &
     shr_mpi_sendr1
   end interface
   interface shr_mpi_recv ; module procedure &
     shr_mpi_recvi0, &
     shr_mpi_recvi1, &
     shr_mpi_recvr0, &
     shr_mpi_recvr1
   end interface
   interface shr_mpi_bcast ; module procedure &
     shr_mpi_bcastc0, &
     shr_mpi_bcastc1, &
     shr_mpi_bcastl0, &
     shr_mpi_bcastl1, &
     shr_mpi_bcasti0, &
     shr_mpi_bcasti1, &
     shr_mpi_bcasti2, &
     shr_mpi_bcastr0, &
     shr_mpi_bcastr1, &
     shr_mpi_bcastr2, &
     shr_mpi_bcastr3
   end interface
   interface shr_mpi_sum ; module procedure &
     shr_mpi_sumi0, &
     shr_mpi_sumi1, &
     shr_mpi_sumb0, &
     shr_mpi_sumb1, &
     shr_mpi_sumr0, &
     shr_mpi_sumr1, &
     shr_mpi_sumr2, &
     shr_mpi_sumr3
   end interface
   interface shr_mpi_min ; module procedure &
     shr_mpi_mini0, &
     shr_mpi_mini1, &
     shr_mpi_minr0, &
     shr_mpi_minr1
   end interface
   interface shr_mpi_max ; module procedure &
     shr_mpi_maxi0, &
     shr_mpi_maxi1, &
     shr_mpi_maxr0, &
     shr_mpi_maxr1
   end interface



# 1 "/opt/intel/impi/3.2.0.011/include64/mpif.h" 1
!
! Copyright (C) 2003-2008 Intel Corporation.  All Rights Reserved.
! 
! The source code contained or described herein and all documents
! related to the source code ("Material") are owned by Intel Corporation
! or its suppliers or licensors.  Title to the Material remains with
! Intel Corporation or its suppliers and licensors.  The Material is
! protected by worldwide copyright and trade secret laws and treaty
! provisions.  No part of the Material may be used, copied, reproduced,
! modified, published, uploaded, posted, transmitted, distributed, or
! disclosed in any way without Intel's prior express written permission.
! 
! No license under any patent, copyright, trade secret or other
! intellectual property right is granted to or conferred upon you by
! disclosure or delivery of the Materials, either expressly, by
! implication, inducement, estoppel or otherwise.  Any license under
! such intellectual property rights must be express and approved by
! Intel in writing.
!
!      
!      
!      (C) 2001 by Argonne National Laboratory.
!      See COPYRIGHT in top-level directory.
!      
!      DO NOT EDIT
!      This file created by buildiface 
!      
       INTEGER MPI_SOURCE, MPI_TAG, MPI_ERROR
       PARAMETER (MPI_SOURCE=3,MPI_TAG=4,MPI_ERROR=5)
       INTEGER MPI_STATUS_SIZE
       PARAMETER (MPI_STATUS_SIZE=5)
       INTEGER MPI_STATUS_IGNORE(MPI_STATUS_SIZE)
       INTEGER MPI_STATUSES_IGNORE(MPI_STATUS_SIZE,1)
       INTEGER MPI_ERRCODES_IGNORE(1)
       CHARACTER*1 MPI_ARGVS_NULL(1,1)
       CHARACTER*1 MPI_ARGV_NULL(1)
       INTEGER MPI_SUCCESS
       PARAMETER (MPI_SUCCESS=0)
       INTEGER MPI_ERR_OTHER
       PARAMETER (MPI_ERR_OTHER=15)
       INTEGER MPI_ERR_WIN
       PARAMETER (MPI_ERR_WIN=45)
       INTEGER MPI_ERR_FILE
       PARAMETER (MPI_ERR_FILE=27)
       INTEGER MPI_ERR_COUNT
       PARAMETER (MPI_ERR_COUNT=2)
       INTEGER MPI_ERR_SPAWN
       PARAMETER (MPI_ERR_SPAWN=42)
       INTEGER MPI_ERR_BASE
       PARAMETER (MPI_ERR_BASE=46)
       INTEGER MPI_ERR_RMA_CONFLICT
       PARAMETER (MPI_ERR_RMA_CONFLICT=49)
       INTEGER MPI_ERR_IN_STATUS
       PARAMETER (MPI_ERR_IN_STATUS=17)
       INTEGER MPI_ERR_INFO_KEY
       PARAMETER (MPI_ERR_INFO_KEY=29)
       INTEGER MPI_ERR_LOCKTYPE
       PARAMETER (MPI_ERR_LOCKTYPE=47)
       INTEGER MPI_ERR_OP
       PARAMETER (MPI_ERR_OP=9)
       INTEGER MPI_ERR_ARG
       PARAMETER (MPI_ERR_ARG=12)
       INTEGER MPI_ERR_READ_ONLY
       PARAMETER (MPI_ERR_READ_ONLY=40)
       INTEGER MPI_ERR_SIZE
       PARAMETER (MPI_ERR_SIZE=51)
       INTEGER MPI_ERR_BUFFER
       PARAMETER (MPI_ERR_BUFFER=1)
       INTEGER MPI_ERR_DUP_DATAREP
       PARAMETER (MPI_ERR_DUP_DATAREP=24)
       INTEGER MPI_ERR_UNSUPPORTED_DATAREP
       PARAMETER (MPI_ERR_UNSUPPORTED_DATAREP=43)
       INTEGER MPI_ERR_LASTCODE
       PARAMETER (MPI_ERR_LASTCODE=1073741823)
       INTEGER MPI_ERR_TRUNCATE
       PARAMETER (MPI_ERR_TRUNCATE=14)
       INTEGER MPI_ERR_DISP
       PARAMETER (MPI_ERR_DISP=52)
       INTEGER MPI_ERR_PORT
       PARAMETER (MPI_ERR_PORT=38)
       INTEGER MPI_ERR_INFO_NOKEY
       PARAMETER (MPI_ERR_INFO_NOKEY=31)
       INTEGER MPI_ERR_ASSERT
       PARAMETER (MPI_ERR_ASSERT=53)
       INTEGER MPI_ERR_FILE_EXISTS
       PARAMETER (MPI_ERR_FILE_EXISTS=25)
       INTEGER MPI_ERR_PENDING
       PARAMETER (MPI_ERR_PENDING=18)
       INTEGER MPI_ERR_COMM
       PARAMETER (MPI_ERR_COMM=5)
       INTEGER MPI_ERR_KEYVAL
       PARAMETER (MPI_ERR_KEYVAL=48)
       INTEGER MPI_ERR_NAME
       PARAMETER (MPI_ERR_NAME=33)
       INTEGER MPI_ERR_REQUEST
       PARAMETER (MPI_ERR_REQUEST=19)
       INTEGER MPI_ERR_GROUP
       PARAMETER (MPI_ERR_GROUP=8)
       INTEGER MPI_ERR_TOPOLOGY
       PARAMETER (MPI_ERR_TOPOLOGY=10)
       INTEGER MPI_ERR_TYPE
       PARAMETER (MPI_ERR_TYPE=3)
       INTEGER MPI_ERR_TAG
       PARAMETER (MPI_ERR_TAG=4)
       INTEGER MPI_ERR_INFO_VALUE
       PARAMETER (MPI_ERR_INFO_VALUE=30)
       INTEGER MPI_ERR_NOT_SAME
       PARAMETER (MPI_ERR_NOT_SAME=35)
       INTEGER MPI_ERR_RMA_SYNC
       PARAMETER (MPI_ERR_RMA_SYNC=50)
       INTEGER MPI_ERR_INFO
       PARAMETER (MPI_ERR_INFO=28)
       INTEGER MPI_ERR_NO_MEM
       PARAMETER (MPI_ERR_NO_MEM=34)
       INTEGER MPI_ERR_BAD_FILE
       PARAMETER (MPI_ERR_BAD_FILE=22)
       INTEGER MPI_ERR_FILE_IN_USE
       PARAMETER (MPI_ERR_FILE_IN_USE=26)
       INTEGER MPI_ERR_UNKNOWN
       PARAMETER (MPI_ERR_UNKNOWN=13)
       INTEGER MPI_ERR_UNSUPPORTED_OPERATION
       PARAMETER (MPI_ERR_UNSUPPORTED_OPERATION=44)
       INTEGER MPI_ERR_QUOTA
       PARAMETER (MPI_ERR_QUOTA=39)
       INTEGER MPI_ERR_AMODE
       PARAMETER (MPI_ERR_AMODE=21)
       INTEGER MPI_ERR_ROOT
       PARAMETER (MPI_ERR_ROOT=7)
       INTEGER MPI_ERR_RANK
       PARAMETER (MPI_ERR_RANK=6)
       INTEGER MPI_ERR_DIMS
       PARAMETER (MPI_ERR_DIMS=11)
       INTEGER MPI_ERR_NO_SUCH_FILE
       PARAMETER (MPI_ERR_NO_SUCH_FILE=37)
       INTEGER MPI_ERR_SERVICE
       PARAMETER (MPI_ERR_SERVICE=41)
       INTEGER MPI_ERR_INTERN
       PARAMETER (MPI_ERR_INTERN=16)
       INTEGER MPI_ERR_IO
       PARAMETER (MPI_ERR_IO=32)
       INTEGER MPI_ERR_ACCESS
       PARAMETER (MPI_ERR_ACCESS=20)
       INTEGER MPI_ERR_NO_SPACE
       PARAMETER (MPI_ERR_NO_SPACE=36)
       INTEGER MPI_ERR_CONVERSION
       PARAMETER (MPI_ERR_CONVERSION=23)
       INTEGER MPI_ERRORS_ARE_FATAL
       PARAMETER (MPI_ERRORS_ARE_FATAL=1409286144)
       INTEGER MPI_ERRORS_RETURN
       PARAMETER (MPI_ERRORS_RETURN=1409286145)
       INTEGER MPI_IDENT
       PARAMETER (MPI_IDENT=0)
       INTEGER MPI_CONGRUENT
       PARAMETER (MPI_CONGRUENT=1)
       INTEGER MPI_SIMILAR
       PARAMETER (MPI_SIMILAR=2)
       INTEGER MPI_UNEQUAL
       PARAMETER (MPI_UNEQUAL=3)
       INTEGER MPI_MAX
       PARAMETER (MPI_MAX=1476395009)
       INTEGER MPI_MIN
       PARAMETER (MPI_MIN=1476395010)
       INTEGER MPI_SUM
       PARAMETER (MPI_SUM=1476395011)
       INTEGER MPI_PROD
       PARAMETER (MPI_PROD=1476395012)
       INTEGER MPI_LAND
       PARAMETER (MPI_LAND=1476395013)
       INTEGER MPI_BAND
       PARAMETER (MPI_BAND=1476395014)
       INTEGER MPI_LOR
       PARAMETER (MPI_LOR=1476395015)
       INTEGER MPI_BOR
       PARAMETER (MPI_BOR=1476395016)
       INTEGER MPI_LXOR
       PARAMETER (MPI_LXOR=1476395017)
       INTEGER MPI_BXOR
       PARAMETER (MPI_BXOR=1476395018)
       INTEGER MPI_MINLOC
       PARAMETER (MPI_MINLOC=1476395019)
       INTEGER MPI_MAXLOC
       PARAMETER (MPI_MAXLOC=1476395020)
       INTEGER MPI_REPLACE
       PARAMETER (MPI_REPLACE=1476395021)
       INTEGER MPI_COMM_WORLD
       PARAMETER (MPI_COMM_WORLD=1140850688)
       INTEGER MPI_COMM_SELF
       PARAMETER (MPI_COMM_SELF=1140850689)
       INTEGER MPI_GROUP_EMPTY
       PARAMETER (MPI_GROUP_EMPTY=1207959552)
       INTEGER MPI_COMM_NULL
       PARAMETER (MPI_COMM_NULL=67108864)
       INTEGER MPI_WIN_NULL
       PARAMETER (MPI_WIN_NULL=536870912)
       INTEGER MPI_FILE_NULL
       PARAMETER (MPI_FILE_NULL=0)
       INTEGER MPI_GROUP_NULL
       PARAMETER (MPI_GROUP_NULL=134217728)
       INTEGER MPI_OP_NULL
       PARAMETER (MPI_OP_NULL=402653184)
       INTEGER MPI_DATATYPE_NULL
       PARAMETER (MPI_DATATYPE_NULL=201326592)
       INTEGER MPI_REQUEST_NULL
       PARAMETER (MPI_REQUEST_NULL=738197504)
       INTEGER MPI_ERRHANDLER_NULL
       PARAMETER (MPI_ERRHANDLER_NULL=335544320)
       INTEGER MPI_INFO_NULL
       PARAMETER (MPI_INFO_NULL=469762048)
       INTEGER MPI_TAG_UB
       PARAMETER (MPI_TAG_UB=1681915906)
       INTEGER MPI_HOST
       PARAMETER (MPI_HOST=1681915908)
       INTEGER MPI_IO
       PARAMETER (MPI_IO=1681915910)
       INTEGER MPI_WTIME_IS_GLOBAL
       PARAMETER (MPI_WTIME_IS_GLOBAL=1681915912)
       INTEGER MPI_UNIVERSE_SIZE
       PARAMETER (MPI_UNIVERSE_SIZE=1681915914)
       INTEGER MPI_LASTUSEDCODE
       PARAMETER (MPI_LASTUSEDCODE=1681915916)
       INTEGER MPI_APPNUM
       PARAMETER (MPI_APPNUM=1681915918)
       INTEGER MPI_WIN_BASE
       PARAMETER (MPI_WIN_BASE=1711276034)
       INTEGER MPI_WIN_SIZE
       PARAMETER (MPI_WIN_SIZE=1711276036)
       INTEGER MPI_WIN_DISP_UNIT
       PARAMETER (MPI_WIN_DISP_UNIT=1711276038)
       INTEGER MPI_MAX_ERROR_STRING
       PARAMETER (MPI_MAX_ERROR_STRING=511)
       INTEGER MPI_MAX_NAME_STRING
       PARAMETER (MPI_MAX_NAME_STRING=62)
       INTEGER MPI_MAX_PORT_NAME
       PARAMETER (MPI_MAX_PORT_NAME=255)
       INTEGER MPI_MAX_OBJECT_NAME
       PARAMETER (MPI_MAX_OBJECT_NAME=127)
       INTEGER MPI_MAX_INFO_KEY
       PARAMETER (MPI_MAX_INFO_KEY=254)
       INTEGER MPI_MAX_INFO_VAL
       PARAMETER (MPI_MAX_INFO_VAL=1023)
       INTEGER MPI_MAX_PROCESSOR_NAME
       PARAMETER (MPI_MAX_PROCESSOR_NAME=128-1)
       INTEGER MPI_MAX_DATAREP_STRING
       PARAMETER (MPI_MAX_DATAREP_STRING=127)
       INTEGER MPI_UNDEFINED, MPI_UNDEFINED_RANK
       PARAMETER (MPI_UNDEFINED=(-32766))
       PARAMETER (MPI_UNDEFINED_RANK=(-32766))
       INTEGER MPI_KEYVAL_INVALID
       PARAMETER (MPI_KEYVAL_INVALID=603979776)
       INTEGER MPI_BSEND_OVERHEAD
       PARAMETER (MPI_BSEND_OVERHEAD=95)
       INTEGER MPI_PROC_NULL
       PARAMETER (MPI_PROC_NULL=-1)
       INTEGER MPI_ANY_SOURCE
       PARAMETER (MPI_ANY_SOURCE=-2)
       INTEGER MPI_ANY_TAG
       PARAMETER (MPI_ANY_TAG=-1)
       INTEGER MPI_ROOT
       PARAMETER (MPI_ROOT=-3)
       INTEGER MPI_GRAPH
       PARAMETER (MPI_GRAPH=1)
       INTEGER MPI_CART
       PARAMETER (MPI_CART=2)
       INTEGER MPI_VERSION
       PARAMETER (MPI_VERSION=2)
       INTEGER MPI_SUBVERSION
       PARAMETER (MPI_SUBVERSION=0)
       INTEGER MPI_LOCK_EXCLUSIVE
       PARAMETER (MPI_LOCK_EXCLUSIVE=234)
       INTEGER MPI_LOCK_SHARED
       PARAMETER (MPI_LOCK_SHARED=235)
       INTEGER MPI_COMPLEX
       PARAMETER (MPI_COMPLEX=1275070494)
       INTEGER MPI_DOUBLE_COMPLEX
       PARAMETER (MPI_DOUBLE_COMPLEX=1275072546)
       INTEGER MPI_LOGICAL
       PARAMETER (MPI_LOGICAL=1275069469)
       INTEGER MPI_REAL
       PARAMETER (MPI_REAL=1275069468)
       INTEGER MPI_DOUBLE_PRECISION
       PARAMETER (MPI_DOUBLE_PRECISION=1275070495)
       INTEGER MPI_INTEGER
       PARAMETER (MPI_INTEGER=1275069467)
       INTEGER MPI_2INTEGER
       PARAMETER (MPI_2INTEGER=1275070496)
       INTEGER MPI_2COMPLEX
       PARAMETER (MPI_2COMPLEX=1275072548)
       INTEGER MPI_2DOUBLE_PRECISION
       PARAMETER (MPI_2DOUBLE_PRECISION=1275072547)
       INTEGER MPI_2REAL
       PARAMETER (MPI_2REAL=1275070497)
       INTEGER MPI_2DOUBLE_COMPLEX
       PARAMETER (MPI_2DOUBLE_COMPLEX=1275076645)
       INTEGER MPI_CHARACTER
       PARAMETER (MPI_CHARACTER=1275068698)
       INTEGER MPI_BYTE
       PARAMETER (MPI_BYTE=1275068685)
       INTEGER MPI_UB
       PARAMETER (MPI_UB=1275068433)
       INTEGER MPI_LB
       PARAMETER (MPI_LB=1275068432)
       INTEGER MPI_PACKED
       PARAMETER (MPI_PACKED=1275068687)
       INTEGER MPI_INTEGER1
       PARAMETER (MPI_INTEGER1=1275068717)
       INTEGER MPI_INTEGER2
       PARAMETER (MPI_INTEGER2=1275068975)
       INTEGER MPI_INTEGER4
       PARAMETER (MPI_INTEGER4=1275069488)
       INTEGER MPI_INTEGER8
       PARAMETER (MPI_INTEGER8=1275070513)
       INTEGER MPI_INTEGER16
       PARAMETER (MPI_INTEGER16=MPI_DATATYPE_NULL)
       INTEGER MPI_REAL4
       PARAMETER (MPI_REAL4=1275069479)
       INTEGER MPI_REAL8
       PARAMETER (MPI_REAL8=1275070505)
       INTEGER MPI_REAL16
       PARAMETER (MPI_REAL16=1275072555)
       INTEGER MPI_COMPLEX8
       PARAMETER (MPI_COMPLEX8=1275070504)
       INTEGER MPI_COMPLEX16
       PARAMETER (MPI_COMPLEX16=1275072554)
       INTEGER MPI_COMPLEX32
       PARAMETER (MPI_COMPLEX32=1275076652)
       INTEGER MPI_ADDRESS_KIND, MPI_OFFSET_KIND
       PARAMETER (MPI_ADDRESS_KIND=8)
       PARAMETER (MPI_OFFSET_KIND=8)
       INTEGER MPI_COMBINER_NAMED
       PARAMETER (MPI_COMBINER_NAMED=1)
       INTEGER MPI_COMBINER_DUP
       PARAMETER (MPI_COMBINER_DUP=2)
       INTEGER MPI_COMBINER_CONTIGUOUS
       PARAMETER (MPI_COMBINER_CONTIGUOUS=3)
       INTEGER MPI_COMBINER_VECTOR
       PARAMETER (MPI_COMBINER_VECTOR=4)
       INTEGER MPI_COMBINER_HVECTOR_INTEGER
       PARAMETER (MPI_COMBINER_HVECTOR_INTEGER=5)
       INTEGER MPI_COMBINER_HVECTOR
       PARAMETER (MPI_COMBINER_HVECTOR=6)
       INTEGER MPI_COMBINER_INDEXED
       PARAMETER (MPI_COMBINER_INDEXED=7)
       INTEGER MPI_COMBINER_HINDEXED_INTEGER
       PARAMETER (MPI_COMBINER_HINDEXED_INTEGER=8)
       INTEGER MPI_COMBINER_HINDEXED
       PARAMETER (MPI_COMBINER_HINDEXED=9)
       INTEGER MPI_COMBINER_INDEXED_BLOCK
       PARAMETER (MPI_COMBINER_INDEXED_BLOCK=10)
       INTEGER MPI_COMBINER_STRUCT_INTEGER
       PARAMETER (MPI_COMBINER_STRUCT_INTEGER=11)
       INTEGER MPI_COMBINER_STRUCT
       PARAMETER (MPI_COMBINER_STRUCT=12)
       INTEGER MPI_COMBINER_SUBARRAY
       PARAMETER (MPI_COMBINER_SUBARRAY=13)
       INTEGER MPI_COMBINER_DARRAY
       PARAMETER (MPI_COMBINER_DARRAY=14)
       INTEGER MPI_COMBINER_F90_REAL
       PARAMETER (MPI_COMBINER_F90_REAL=15)
       INTEGER MPI_COMBINER_F90_COMPLEX
       PARAMETER (MPI_COMBINER_F90_COMPLEX=16)
       INTEGER MPI_COMBINER_F90_INTEGER
       PARAMETER (MPI_COMBINER_F90_INTEGER=17)
       INTEGER MPI_COMBINER_RESIZED
       PARAMETER (MPI_COMBINER_RESIZED=18)
       INTEGER MPI_MODE_NOCHECK
       PARAMETER (MPI_MODE_NOCHECK=1024)
       INTEGER MPI_MODE_NOSTORE
       PARAMETER (MPI_MODE_NOSTORE=2048)
       INTEGER MPI_MODE_NOPUT
       PARAMETER (MPI_MODE_NOPUT=4096)
       INTEGER MPI_MODE_NOPRECEDE
       PARAMETER (MPI_MODE_NOPRECEDE=8192)
       INTEGER MPI_MODE_NOSUCCEED
       PARAMETER (MPI_MODE_NOSUCCEED=16384)
       INTEGER MPI_THREAD_SINGLE
       PARAMETER (MPI_THREAD_SINGLE=0)
       INTEGER MPI_THREAD_FUNNELED
       PARAMETER (MPI_THREAD_FUNNELED=1)
       INTEGER MPI_THREAD_SERIALIZED
       PARAMETER (MPI_THREAD_SERIALIZED=2)
       INTEGER MPI_THREAD_MULTIPLE
       PARAMETER (MPI_THREAD_MULTIPLE=3)
       INTEGER MPI_MODE_RDONLY
       PARAMETER (MPI_MODE_RDONLY=2)
       INTEGER MPI_MODE_RDWR
       PARAMETER (MPI_MODE_RDWR=8)
       INTEGER MPI_MODE_WRONLY
       PARAMETER (MPI_MODE_WRONLY=4)
       INTEGER MPI_MODE_DELETE_ON_CLOSE
       PARAMETER (MPI_MODE_DELETE_ON_CLOSE=16)
       INTEGER MPI_MODE_UNIQUE_OPEN
       PARAMETER (MPI_MODE_UNIQUE_OPEN=32)
       INTEGER MPI_MODE_CREATE
       PARAMETER (MPI_MODE_CREATE=1)
       INTEGER MPI_MODE_EXCL
       PARAMETER (MPI_MODE_EXCL=64)
       INTEGER MPI_MODE_APPEND
       PARAMETER (MPI_MODE_APPEND=128)
       INTEGER MPI_MODE_SEQUENTIAL
       PARAMETER (MPI_MODE_SEQUENTIAL=256)
       INTEGER MPI_SEEK_SET
       PARAMETER (MPI_SEEK_SET=600)
       INTEGER MPI_SEEK_CUR
       PARAMETER (MPI_SEEK_CUR=602)
       INTEGER MPI_SEEK_END
       PARAMETER (MPI_SEEK_END=604)
       INTEGER MPI_ORDER_C
       PARAMETER (MPI_ORDER_C=56)
       INTEGER MPI_ORDER_FORTRAN
       PARAMETER (MPI_ORDER_FORTRAN=57)
       INTEGER MPI_DISTRIBUTE_BLOCK
       PARAMETER (MPI_DISTRIBUTE_BLOCK=121)
       INTEGER MPI_DISTRIBUTE_CYCLIC
       PARAMETER (MPI_DISTRIBUTE_CYCLIC=122)
       INTEGER MPI_DISTRIBUTE_NONE
       PARAMETER (MPI_DISTRIBUTE_NONE=123)
       INTEGER MPI_DISTRIBUTE_DFLT_DARG
       PARAMETER (MPI_DISTRIBUTE_DFLT_DARG=-49767)
       INTEGER MPI_DISPLACEMENT_CURRENT
       PARAMETER (MPI_DISPLACEMENT_CURRENT=-54278278)
       INTEGER MPI_BOTTOM, MPI_IN_PLACE
       EXTERNAL MPI_DUP_FN, MPI_NULL_DELETE_FN, MPI_NULL_COPY_FN
       EXTERNAL MPI_WTIME, MPI_WTICK
       EXTERNAL PMPI_WTIME, PMPI_WTICK
       EXTERNAL MPI_COMM_DUP_FN, MPI_COMM_NULL_DELETE_FN
       EXTERNAL MPI_COMM_NULL_COPY_FN
       EXTERNAL MPI_WIN_DUP_FN, MPI_WIN_NULL_DELETE_FN
       EXTERNAL MPI_WIN_NULL_COPY_FN
       EXTERNAL MPI_TYPE_DUP_FN, MPI_TYPE_NULL_DELETE_FN
       EXTERNAL MPI_TYPE_NULL_COPY_FN
       EXTERNAL MPI_CONVERSION_FN_NULL
       DOUBLE PRECISION MPI_WTIME, MPI_WTICK
       DOUBLE PRECISION PMPI_WTIME, PMPI_WTICK


       COMMON /MPIPRIV1/ MPI_BOTTOM, MPI_IN_PLACE, MPI_STATUS_IGNORE

       COMMON /MPIPRIV2/ MPI_STATUSES_IGNORE, MPI_ERRCODES_IGNORE
       SAVE /MPIPRIV1/,/MPIPRIV2/

       COMMON /MPIPRIVC/ MPI_ARGVS_NULL, MPI_ARGV_NULL
       SAVE   /MPIPRIVC/
# 80 "/data3/work/yuxinzhu/test/GAMIL2.8_AMIP-sole/run/lib/c_coupler/src/individual_src_files/shr_mpi_mod.F90" 2


CONTAINS

!===============================================================================

SUBROUTINE shr_mpi_chkerr(rcode,string)

   IMPLICIT none

   !----- arguments ---
   integer(SHR_KIND_IN), intent(in) :: rcode  ! input MPI error code
   character(*),         intent(in) :: string ! message

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_chkerr) '

   character(MPI_MAX_ERROR_STRING)  :: lstring

   integer(SHR_KIND_IN)             :: len

!-------------------------------------------------------------------------------
! PURPOSE: layer on MPI error checking
!-------------------------------------------------------------------------------


   if (rcode /= MPI_SUCCESS) then
!    call MPI_ERROR_STRING(rcode,lstring,len)
!    write(6,*) trim(subName),":",lstring(1:len)
     write(6,*) trim(subName),":",trim(string)
     call shr_mpi_abort(subName)
   endif


END SUBROUTINE shr_mpi_chkerr

!===============================================================================

SUBROUTINE shr_mpi_sendi0(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   integer(SHR_KIND_IN), intent(in) :: lvec     ! send value
   integer(SHR_KIND_IN), intent(in) :: pid      ! pid to send to
   integer(SHR_KIND_IN), intent(in) :: tag      ! tag
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_sendi0) '
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Send a vector of reals
!-------------------------------------------------------------------------------


   lsize = 1

   call MPI_SEND(lvec,lsize,MPI_INTEGER,pid,tag,comm,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif


END SUBROUTINE shr_mpi_sendi0

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_sendi1(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   integer(SHR_KIND_IN), intent(in) :: lvec(:)  ! in/out local values
   integer(SHR_KIND_IN), intent(in) :: pid      ! pid to send to
   integer(SHR_KIND_IN), intent(in) :: tag      ! tag
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_sendi1) '
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Send a vector of reals
!-------------------------------------------------------------------------------


   lsize = size(lvec)

   call MPI_SEND(lvec,lsize,MPI_INTEGER,pid,tag,comm,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif



END SUBROUTINE shr_mpi_sendi1

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_sendr0(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   real(SHR_KIND_R8),    intent(in) :: lvec     ! in/out local values
   integer(SHR_KIND_IN), intent(in) :: pid      ! pid to send to
   integer(SHR_KIND_IN), intent(in) :: tag      ! tag
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_sendr0) '
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Send a vector of reals
!-------------------------------------------------------------------------------


   lsize = 1

   call MPI_SEND(lvec,lsize,MPI_REAL8,pid,tag,comm,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif


END SUBROUTINE shr_mpi_sendr0

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_sendr1(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   real(SHR_KIND_R8),    intent(in) :: lvec(:)  ! in/out local values
   integer(SHR_KIND_IN), intent(in) :: pid      ! pid to send to
   integer(SHR_KIND_IN), intent(in) :: tag      ! tag
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_sendr1) '
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Send a vector of reals
!-------------------------------------------------------------------------------


   lsize = size(lvec)

   call MPI_SEND(lvec,lsize,MPI_REAL8,pid,tag,comm,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif


END SUBROUTINE shr_mpi_sendr1

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_recvi0(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   integer(SHR_KIND_IN), intent(out):: lvec     ! in/out local values
   integer(SHR_KIND_IN), intent(in) :: pid      ! pid to recv from
   integer(SHR_KIND_IN), intent(in) :: tag      ! tag
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_recvi0) '
   integer(SHR_KIND_IN)             :: lsize

   integer(SHR_KIND_IN)             :: status(MPI_STATUS_SIZE)  ! mpi status info

   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Recv a vector of reals
!-------------------------------------------------------------------------------


   lsize = 1

   call MPI_RECV(lvec,lsize,MPI_INTEGER,pid,tag,comm,status,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif


END SUBROUTINE shr_mpi_recvi0

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_recvi1(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   integer(SHR_KIND_IN), intent(out):: lvec(:)  ! in/out local values
   integer(SHR_KIND_IN), intent(in) :: pid      ! pid to recv from
   integer(SHR_KIND_IN), intent(in) :: tag      ! tag
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_recvi1) '
   integer(SHR_KIND_IN)             :: lsize

   integer(SHR_KIND_IN)             :: status(MPI_STATUS_SIZE)  ! mpi status info

   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Recv a vector of reals
!-------------------------------------------------------------------------------


   lsize = size(lvec)

   call MPI_RECV(lvec,lsize,MPI_INTEGER,pid,tag,comm,status,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif


END SUBROUTINE shr_mpi_recvi1

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_recvr0(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   real(SHR_KIND_R8),    intent(out):: lvec     ! in/out local values
   integer(SHR_KIND_IN), intent(in) :: pid      ! pid to recv from
   integer(SHR_KIND_IN), intent(in) :: tag      ! tag
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_recvr0) '
   integer(SHR_KIND_IN)             :: lsize

   integer(SHR_KIND_IN)             :: status(MPI_STATUS_SIZE)  ! mpi status info

   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Recv a vector of reals
!-------------------------------------------------------------------------------


   lsize = 1

   call MPI_RECV(lvec,lsize,MPI_REAL8,pid,tag,comm,status,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif


END SUBROUTINE shr_mpi_recvr0

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_recvr1(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   real(SHR_KIND_R8),    intent(out):: lvec(:)  ! in/out local values
   integer(SHR_KIND_IN), intent(in) :: pid      ! pid to recv from
   integer(SHR_KIND_IN), intent(in) :: tag      ! tag
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_recvr1) '
   integer(SHR_KIND_IN)             :: lsize

   integer(SHR_KIND_IN)             :: status(MPI_STATUS_SIZE)  ! mpi status info

   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Recv a vector of reals
!-------------------------------------------------------------------------------


   lsize = size(lvec)

   call MPI_RECV(lvec,lsize,MPI_REAL8,pid,tag,comm,status,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif


END SUBROUTINE shr_mpi_recvr1

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_bcasti0(vec,comm,string)

   IMPLICIT none

   !----- arguments ---
   integer(SHR_KIND_IN), intent(inout):: vec      ! vector of 1
   integer(SHR_KIND_IN), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message

   !----- local ---
   character(*),parameter             :: subName = '(shr_mpi_bcasti0) '
   integer(SHR_KIND_IN)               :: ierr
   integer(SHR_KIND_IN)               :: lsize

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast an integer
!-------------------------------------------------------------------------------


   lsize = 1

   call MPI_BCAST(vec,lsize,MPI_INTEGER,0,comm,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif


END SUBROUTINE shr_mpi_bcasti0

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_bcastr0(vec,comm,string)

   IMPLICIT none

   !----- arguments ---
   real(SHR_KIND_R8),    intent(inout):: vec      ! vector of 1
   integer(SHR_KIND_IN), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message

   !----- local ---
   character(*),parameter             :: subName = '(shr_mpi_bcastr0) '
   integer(SHR_KIND_IN)               :: ierr
   integer(SHR_KIND_IN)               :: lsize

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a real
!-------------------------------------------------------------------------------

   lsize = 1

   call MPI_BCAST(vec,lsize,MPI_REAL8,0,comm,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif


END SUBROUTINE shr_mpi_bcastr0

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_bcastl0(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   logical, intent(inout):: vec      ! vector of 1
   integer(SHR_KIND_IN), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message
   integer(SHR_KIND_IN), optional, intent(in)   :: pebcast  ! bcast pe (otherwise zero)

   !----- local ---
   character(*),parameter             :: subName = '(shr_mpi_bcastl0) '
   integer(SHR_KIND_IN)               :: ierr
   integer(SHR_KIND_IN)               :: lsize
   integer(SHR_KIND_IN)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a logical
!-------------------------------------------------------------------------------

   lsize = 1
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_LOGICAL,lpebcast,comm,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif

END SUBROUTINE shr_mpi_bcastl0

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_bcastc0(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   character(len=*), intent(inout)    :: vec      ! vector of 1
   integer(SHR_KIND_IN), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message
   integer(SHR_KIND_IN), optional, intent(in)   :: pebcast  ! bcast pe (otherwise zero)

   !----- local ---
   character(*),parameter             :: subName = '(shr_mpi_bcastc0) '
   integer(SHR_KIND_IN)               :: ierr
   integer(SHR_KIND_IN)               :: lsize
   integer(SHR_KIND_IN)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a character string
!-------------------------------------------------------------------------------

   lsize = len(vec)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_CHARACTER,lpebcast,comm,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif

END SUBROUTINE shr_mpi_bcastc0

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_bcasti1(vec,comm,string)

   IMPLICIT none

   !----- arguments ---
   integer(SHR_KIND_IN), intent(inout):: vec(:)   ! vector 
   integer(SHR_KIND_IN), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message

   !----- local ---
   character(*),parameter             :: subName = '(shr_mpi_bcasti1) '
   integer(SHR_KIND_IN)               :: ierr
   integer(SHR_KIND_IN)               :: lsize

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a vector of integers
!-------------------------------------------------------------------------------


   lsize = size(vec)

   call MPI_BCAST(vec,lsize,MPI_INTEGER,0,comm,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif


END SUBROUTINE shr_mpi_bcasti1

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_bcastr1(vec,comm,string)

   IMPLICIT none

   !----- arguments ---
   real(SHR_KIND_R8),    intent(inout):: vec(:)   ! vector 
   integer(SHR_KIND_IN), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message

   !----- local ---
   character(*),parameter             :: subName = '(shr_mpi_bcastr1) '
   integer(SHR_KIND_IN)               :: ierr
   integer(SHR_KIND_IN)               :: lsize

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a vector of reals
!-------------------------------------------------------------------------------


   lsize = size(vec)

   call MPI_BCAST(vec,lsize,MPI_REAL8,0,comm,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif


END SUBROUTINE shr_mpi_bcastr1

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_bcastc1(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   character(len=*), intent(inout)    :: vec(:)   ! 1D vector
   integer(SHR_KIND_IN), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message
   integer(SHR_KIND_IN), optional, intent(in)   :: pebcast  ! bcast pe (otherwise zero)

   !----- local ---
   character(*),parameter             :: subName = '(shr_mpi_bcastc1) '
   integer(SHR_KIND_IN)               :: ierr
   integer(SHR_KIND_IN)               :: lsize
   integer(SHR_KIND_IN)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a character string
!-------------------------------------------------------------------------------

   lsize = size(vec)*len(vec)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_CHARACTER,lpebcast,comm,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif

END SUBROUTINE shr_mpi_bcastc1

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_bcastl1(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   logical, intent(inout):: vec(:)      ! vector of 1
   integer(SHR_KIND_IN), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message
   integer(SHR_KIND_IN), optional, intent(in)   :: pebcast  ! bcast pe (otherwise zero)

   !----- local ---
   character(*),parameter             :: subName = '(shr_mpi_bcastl1) '
   integer(SHR_KIND_IN)               :: ierr
   integer(SHR_KIND_IN)               :: lsize
   integer(SHR_KIND_IN)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a logical
!-------------------------------------------------------------------------------

   lsize = size(vec)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_LOGICAL,lpebcast,comm,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif

END SUBROUTINE shr_mpi_bcastl1

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_bcasti2(arr,comm,string,pebcast)

   IMPLICIT none

   !----- arguments -----
   integer,              intent(inout):: arr(:,:) ! array, 2d 
   integer(SHR_KIND_IN), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message
   integer(SHR_KIND_IN), optional, intent(in)   :: pebcast  ! bcast pe (otherwise zero)

   !----- local -----
   integer(SHR_KIND_IN)               :: ierr
   integer(SHR_KIND_IN)               :: lsize
   integer(SHR_KIND_IN)               :: lpebcast

   !----- formats -----
   character(*),parameter             :: subName = '(shr_mpi_bcasti2) '

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a 2d array of integers
!-------------------------------------------------------------------------------

   lsize = size(arr)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(arr,lsize,MPI_INTEGER,lpebcast,comm,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif

END SUBROUTINE shr_mpi_bcasti2

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_bcastr2(arr,comm,string,pebcast)

   IMPLICIT none

   !----- arguments -----
   real(SHR_KIND_R8),    intent(inout):: arr(:,:) ! array, 2d 
   integer(SHR_KIND_IN), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message
   integer(SHR_KIND_IN), optional, intent(in)   :: pebcast  ! bcast pe (otherwise zero)

   !----- local -----
   integer(SHR_KIND_IN)               :: ierr
   integer(SHR_KIND_IN)               :: lsize
   integer(SHR_KIND_IN)               :: lpebcast

   !----- formats -----
   character(*),parameter             :: subName = '(shr_mpi_bcastr2) '

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a 2d array of reals
!-------------------------------------------------------------------------------

   lsize = size(arr)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(arr,lsize,MPI_REAL8,lpebcast,comm,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif

END SUBROUTINE shr_mpi_bcastr2

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_bcastr3(arr,comm,string,pebcast)

   IMPLICIT none

   !----- arguments -----
   real(SHR_KIND_R8),    intent(inout):: arr(:,:,:) ! array, 3d 
   integer(SHR_KIND_IN), intent(in)   :: comm       ! mpi communicator
   character(*),optional,intent(in)   :: string     ! message
   integer(SHR_KIND_IN), optional, intent(in)   :: pebcast  ! bcast pe (otherwise zero)

   !----- local -----
   integer(SHR_KIND_IN)               :: ierr
   integer(SHR_KIND_IN)               :: lsize
   integer(SHR_KIND_IN)               :: lpebcast

   !----- formats -----
   character(*),parameter             :: subName = '(shr_mpi_bcastr3) '

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a 3d array of reals
!-------------------------------------------------------------------------------

   lsize = size(arr)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(arr,lsize,MPI_REAL8,lpebcast,comm,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif

END SUBROUTINE shr_mpi_bcastr3


!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_sumi0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(SHR_KIND_IN), intent(in) :: lvec     ! in/out local values
   integer(SHR_KIND_IN), intent(out):: gvec     ! in/out global values
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_sumi0) '
   logical                          :: lall
   character(SHR_KIND_CL)           :: lstring
   integer(SHR_KIND_IN)             :: reduce_type  ! mpi reduction type
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: gsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------


   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call shr_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,0,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif


END SUBROUTINE shr_mpi_sumi0

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_sumi1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(SHR_KIND_IN), intent(in) :: lvec(:)  ! in/out local values
   integer(SHR_KIND_IN), intent(out):: gvec(:)  ! in/out global values
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_sumi1) '
   logical                          :: lall
   character(SHR_KIND_CL)           :: lstring
   integer(SHR_KIND_IN)             :: reduce_type  ! mpi reduction type
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: gsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------


   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call shr_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,0,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif


END SUBROUTINE shr_mpi_sumi1

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_sumb0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(SHR_KIND_I8), intent(in) :: lvec     ! in/out local values
   integer(SHR_KIND_I8), intent(out):: gvec     ! in/out global values
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_sumb0) '
   logical                          :: lall
   character(SHR_KIND_CL)           :: lstring
   integer(SHR_KIND_IN)             :: reduce_type  ! mpi reduction type
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: gsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------

   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call shr_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER8,reduce_type,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER8,reduce_type,0,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

END SUBROUTINE shr_mpi_sumb0

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_sumb1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(SHR_KIND_I8), intent(in) :: lvec(:)  ! in/out local values
   integer(SHR_KIND_I8), intent(out):: gvec(:)  ! in/out global values
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_sumb1) '
   logical                          :: lall
   character(SHR_KIND_CL)           :: lstring
   integer(SHR_KIND_IN)             :: reduce_type  ! mpi reduction type
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: gsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------

   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call shr_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER8,reduce_type,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER8,reduce_type,0,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

END SUBROUTINE shr_mpi_sumb1

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_sumr0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(SHR_KIND_R8),    intent(in) :: lvec     ! in/out local values
   real(SHR_KIND_R8),    intent(out):: gvec     ! in/out global values
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_sumr0) '
   logical                          :: lall
   character(SHR_KIND_CL)           :: lstring
   integer(SHR_KIND_IN)             :: reduce_type  ! mpi reduction type
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: gsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------


   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call shr_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif


END SUBROUTINE shr_mpi_sumr0

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_sumr1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(SHR_KIND_R8),    intent(in) :: lvec(:)  ! in/out local values
   real(SHR_KIND_R8),    intent(out):: gvec(:)  ! in/out global values
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_sumr1) '
   logical                          :: lall
   character(SHR_KIND_CL)           :: lstring
   integer(SHR_KIND_IN)             :: reduce_type  ! mpi reduction type
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: gsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------


   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call shr_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif


END SUBROUTINE shr_mpi_sumr1

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_sumr2(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(SHR_KIND_R8),    intent(in) :: lvec(:,:)! in/out local values
   real(SHR_KIND_R8),    intent(out):: gvec(:,:)! in/out global values
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_sumr2) '
   logical                          :: lall
   character(SHR_KIND_CL)           :: lstring
   integer(SHR_KIND_IN)             :: reduce_type  ! mpi reduction type
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: gsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------


   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call shr_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif


END SUBROUTINE shr_mpi_sumr2

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_sumr3(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(SHR_KIND_R8),    intent(in) :: lvec(:,:,:) ! in/out local values
   real(SHR_KIND_R8),    intent(out):: gvec(:,:,:) ! in/out global values
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_sumr3) '
   logical                          :: lall
   character(SHR_KIND_CL)           :: lstring
   integer(SHR_KIND_IN)             :: reduce_type  ! mpi reduction type
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: gsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------


   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call shr_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif


END SUBROUTINE shr_mpi_sumr3

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_mini0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(SHR_KIND_IN), intent(in) :: lvec     ! in/out local values
   integer(SHR_KIND_IN), intent(out):: gvec     ! in/out global values
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_mini0) '
   logical                          :: lall
   character(SHR_KIND_CL)           :: lstring
   integer(SHR_KIND_IN)             :: reduce_type  ! mpi reduction type
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: gsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds min of a distributed vector of values, assume local min
!          already computed
!-------------------------------------------------------------------------------


   reduce_type = MPI_MIN
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call shr_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,0,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif


END SUBROUTINE shr_mpi_mini0

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_mini1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(SHR_KIND_IN), intent(in) :: lvec(:)  ! in/out local values
   integer(SHR_KIND_IN), intent(out):: gvec(:)  ! in/out global values
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_mini1) '
   logical                          :: lall
   character(SHR_KIND_CL)           :: lstring
   integer(SHR_KIND_IN)             :: reduce_type  ! mpi reduction type
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: gsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds min of a distributed vector of values, assume local min
!          already computed
!-------------------------------------------------------------------------------


   reduce_type = MPI_MIN
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call shr_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,0,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif


END SUBROUTINE shr_mpi_mini1

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_minr0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(SHR_KIND_R8),    intent(in) :: lvec     ! in/out local values
   real(SHR_KIND_R8),    intent(out):: gvec     ! in/out global values
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_minr0) '
   logical                          :: lall
   character(SHR_KIND_CL)           :: lstring
   integer(SHR_KIND_IN)             :: reduce_type  ! mpi reduction type
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: gsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds min of a distributed vector of values, assume local min
!          already computed
!-------------------------------------------------------------------------------


   reduce_type = MPI_MIN
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call shr_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif


END SUBROUTINE shr_mpi_minr0

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_minr1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(SHR_KIND_R8),    intent(in) :: lvec(:)  ! in/out local values
   real(SHR_KIND_R8),    intent(out):: gvec(:)  ! in/out global values
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_minr1) '
   logical                          :: lall
   character(SHR_KIND_CL)           :: lstring
   integer(SHR_KIND_IN)             :: reduce_type  ! mpi reduction type
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: gsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds min of a distributed vector of values, assume local min
!          already computed
!-------------------------------------------------------------------------------


   reduce_type = MPI_MIN
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call shr_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif


END SUBROUTINE shr_mpi_minr1

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_maxi0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(SHR_KIND_IN), intent(in) :: lvec     ! in/out local values
   integer(SHR_KIND_IN), intent(out):: gvec     ! in/out global values
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_maxi0) '
   logical                          :: lall
   character(SHR_KIND_CL)           :: lstring
   integer(SHR_KIND_IN)             :: reduce_type  ! mpi reduction type
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: gsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds max of a distributed vector of values, assume local max
!          already computed
!-------------------------------------------------------------------------------


   reduce_type = MPI_MAX
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call shr_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,0,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif


END SUBROUTINE shr_mpi_maxi0

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_maxi1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(SHR_KIND_IN), intent(in) :: lvec(:)  ! in/out local values
   integer(SHR_KIND_IN), intent(out):: gvec(:)  ! in/out global values
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_maxi1) '
   logical                          :: lall
   character(SHR_KIND_CL)           :: lstring
   integer(SHR_KIND_IN)             :: reduce_type  ! mpi reduction type
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: gsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds max of a distributed vector of values, assume local max
!          already computed
!-------------------------------------------------------------------------------


   reduce_type = MPI_MAX
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call shr_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,0,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif


END SUBROUTINE shr_mpi_maxi1

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_maxr0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(SHR_KIND_R8),    intent(in) :: lvec     ! in/out local values
   real(SHR_KIND_R8),    intent(out):: gvec     ! in/out global values
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_maxr0) '
   logical                          :: lall
   character(SHR_KIND_CL)           :: lstring
   integer(SHR_KIND_IN)             :: reduce_type  ! mpi reduction type
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: gsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds max of a distributed vector of values, assume local max
!          already computed
!-------------------------------------------------------------------------------


   reduce_type = MPI_MAX
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call shr_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif


END SUBROUTINE shr_mpi_maxr0

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_maxr1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(SHR_KIND_R8),    intent(in) :: lvec(:)  ! in/out local values
   real(SHR_KIND_R8),    intent(out):: gvec(:)  ! in/out global values
   integer(SHR_KIND_IN), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = '(shr_mpi_maxr1) '
   logical                          :: lall
   character(SHR_KIND_CL)           :: lstring
   integer(SHR_KIND_IN)             :: reduce_type  ! mpi reduction type
   integer(SHR_KIND_IN)             :: lsize
   integer(SHR_KIND_IN)             :: gsize
   integer(SHR_KIND_IN)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds max of a distributed vector of values, assume local max
!          already computed
!-------------------------------------------------------------------------------


   reduce_type = MPI_MAX
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call shr_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call shr_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif


END SUBROUTINE shr_mpi_maxr1

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_commsize(comm,size,string)

   IMPLICIT none

   !----- arguments ---
   integer,intent(in)                 :: comm
   integer,intent(out)                :: size
   character(*),optional,intent(in)   :: string   ! message

   !----- local ---
   character(*),parameter             :: subName = '(shr_mpi_commsize) '
   integer(SHR_KIND_IN)               :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI commsize
!-------------------------------------------------------------------------------


   call MPI_COMM_SIZE(comm,size,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif


END SUBROUTINE shr_mpi_commsize

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_commrank(comm,rank,string)

   IMPLICIT none

   !----- arguments ---
   integer,intent(in)                 :: comm
   integer,intent(out)                :: rank
   character(*),optional,intent(in)   :: string   ! message

   !----- local ---
   character(*),parameter             :: subName = '(shr_mpi_commrank) '
   integer(SHR_KIND_IN)               :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI commrank
!-------------------------------------------------------------------------------


   call MPI_COMM_RANK(comm,rank,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif


END SUBROUTINE shr_mpi_commrank

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_initialized(flag,string)

   IMPLICIT none

   !----- arguments ---
   logical,intent(out)                :: flag
   character(*),optional,intent(in)   :: string   ! message

   !----- local ---
   character(*),parameter             :: subName = '(shr_mpi_initialized) '
   integer(SHR_KIND_IN)               :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI initialized
!-------------------------------------------------------------------------------


   call MPI_INITIALIZED(flag,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif


END SUBROUTINE shr_mpi_initialized

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_abort(string,rcode)

   IMPLICIT none

   !----- arguments ---
   character(*),optional,intent(in)   :: string   ! message
   integer,optional,intent(in)        :: rcode    ! optional code

   !----- local ---
   character(*),parameter             :: subName = '(shr_mpi_abort) '
   integer(SHR_KIND_IN)               :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI abort
!-------------------------------------------------------------------------------


   write(6,*) trim(subName),":",trim(string),rcode
   call MPI_ABORT(MPI_COMM_WORLD,rcode,ierr)


END SUBROUTINE shr_mpi_abort

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_barrier(comm,string)

   IMPLICIT none

   !----- arguments ---
   integer,intent(in)                 :: comm
   character(*),optional,intent(in)   :: string   ! message

   !----- local ---
   character(*),parameter             :: subName = '(shr_mpi_barrier) '
   integer(SHR_KIND_IN)               :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI barrier
!-------------------------------------------------------------------------------


   call MPI_BARRIER(comm,ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif


END SUBROUTINE shr_mpi_barrier

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_init(string)

   IMPLICIT none

   !----- arguments ---
   character(*),optional,intent(in)   :: string   ! message

   !----- local ---
   character(*),parameter             :: subName = '(shr_mpi_init) '
   integer(SHR_KIND_IN)               :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI init
!-------------------------------------------------------------------------------


   call MPI_INIT(ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif


END SUBROUTINE shr_mpi_init

!===============================================================================
!===============================================================================

SUBROUTINE shr_mpi_finalize(string)

   IMPLICIT none

   !----- arguments ---
   character(*),optional,intent(in)   :: string   ! message

   !----- local ---
   character(*),parameter             :: subName = '(shr_mpi_finalize) '
   integer(SHR_KIND_IN)               :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI finalize
!-------------------------------------------------------------------------------


   call MPI_BARRIER(MPI_COMM_WORLD,ierr)
   call MPI_FINALIZE(ierr)
   if (present(string)) then
     call shr_mpi_chkerr(ierr,subName//trim(string))
   else
     call shr_mpi_chkerr(ierr,subName)
   endif


END SUBROUTINE shr_mpi_finalize

!===============================================================================
!===============================================================================

END MODULE shr_mpi_mod
