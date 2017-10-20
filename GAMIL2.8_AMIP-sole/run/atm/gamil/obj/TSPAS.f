# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/TSPAS.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/TSPAS.F"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/TSPAS.F" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/TSPAS.F" 2




      SUBROUTINE TSPAS(Q,U,V,P,DSNP,DSSP,GC,DTDLT,DTDLN)

!     ==================
!     TWO-STEP SHAPE-PRESERVING ADVECTION SCHEME DEVELOPED
!     BY YU RUCONG
!     ==================

!     PERFORM 2-D ADVECTION IN GLOBAL SPHERICAL GEOMETRY
!     WITH THE UNIFORM LAT-LON C-GRID MESH
!
      use mpishorthand, only: mpicom

      IMPLICIT NONE


# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/PARADYN" 1



!     Define the parameters related to the model resolution


      integer nprocessor
      parameter(nprocessor=20)            !by LPF


      INTEGER
     _        IM   ! the grid number along the longitude
     _       ,NX   ! NX = IM+2, considering the periodic boundary condition
     _       ,NY   ! the grid nmuber along the latitude
     _       ,NL   ! the vertical layers
     _       ,NZ   ! NZ = NL + 1, considering the adding boundary in the top atmosphere
     _       ,NA




      PARAMETER(IM=128,NX=IM+2,NY=60/nprocessor+2,NL=26,NZ=NL+1)


!     Define the paramters about the earth and the atmosphere, required by
!     the model atmosphere
!
      REAL*8
     _       RAD    ! the earth radius
     _      ,OMGA   ! the angular velocity of the earth	rotation
     _      ,GRAVIT ! the gravity
     _      ,RD     ! the dry air specific gas constant
     _      ,CP     ! specific heat at constant pressure
     _      ,CPD    ! specific heat at constant pressure
     _      ,CAPA   ! CAPA=RD/CP
!     _      ,P0    ! The sea level pressure of the standard atmosphere
!     _      ,T0    ! The sea level temperature of the standard atmosphere
     _      ,PI     ! the ratio of the circumference of a circle to its diameter
     _      ,PEALIB ! the maxium pressure of the standard atmoshere
     _      ,DPALIB ! the interval of two adjoining levels
!
      PARAMETER(RAD=6371000.0D0, OMGA=0.7292D-4, GRAVIT=9.806D0
!     _         ,RD =287.0D0,CP=1004.6D0,CAPA=RD/CP,T0=288.15D0
!     _         ,P0 =1013.25D0, PI=3.141592653589793D0)
     _         ,RD =287.0D0,CP=1004.6D0,CAPA=RD/CP,CPD=CP
     _         ,PI=3.141592653589793D0)
!      PARAMETER ( PEALIB=1160.0D0,DPALIB=2.5D0,NA=PEALIB/DPALIB )
*     PARAMETER ( PEALIB=1160.0D0,DPALIB=5.0D0,NA=PEALIB/DPALIB )
      PARAMETER ( PEALIB=1160.0D0,DPALIB=0.5D0,NA=PEALIB/DPALIB )
!
# 22 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/TSPAS.F" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/PARADD" 1
      INTEGER IB,IE,JB,JE,KE,NM
      PARAMETER ( IB=2,IE=NX-1,JB=2,JE=NY-1,KE=NL+2,NM=NL-1 )
# 23 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/TSPAS.F" 2


# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/commpi.h" 1

        
      integer nprocs, myrank, ierr, itop, ibot, jbeg, jend, jpole
      logical inc_pole

      common/commpi/ nprocs, myrank, itop, ibot, jbeg, jend, jpole, inc_pole
# 25 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/TSPAS.F" 2

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
# 26 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/TSPAS.F" 2

      character*50 filename

      integer isend11,irecv11
      integer isend12,irecv12
      integer isend22,irecv22
      integer istatus(mpi_status_size)

      real*8 workst1(nx*nl),workrb1(nx*nl)
      real*8 worksb1(nx*nl),workrt1(nx*nl)
      real*8 worksb2(nx*(nl+1)),workrt2(nx*(nl+1))
      integer ii


!
      REAL*8 Q(NX,NY,NL),U(NX,NY,NL),V(NX,NY,NL),P(NX,NY)
!
      REAL*8 QMIN(NX,NY,NL),QMAX(NX,NY,NL)
!
!!      REAL*8 HU(NX,NY),HV(NX,NY),CU(NX,NY),CV(NX,NY)
!!     _      ,UU(NX,NY),VV(NX,NY),H(NX,NY),A(NX,NY)
!!     _      ,QH(NX,NY),QHSTAR(NX,NY),USTAR(NX,NY),VSTAR(NX,NY)
!!      REAL*8 BETA(NX,NY),FX(NX,NY),FY(NX,NY),C(NX,2)

      REAL*8 HU(NX,NY),HV(NX,NY),CU(NX,NY),CV(NX,NY),H(NX,NY)

      real*8 UU(NX,NY,nl),VV(NX,NY,nl),A(NX,NY,nl)
      real*8 QH(NX,NY,nl),QHSTAR(NX,NY,nl),USTAR(NX,NY,nl),VSTAR(NX,NY,nl)
      REAL*8 BETA(NX,NY,nl),FX(NX,NY,nl),FY(NX,NY,nl),C(NX,2,nl)
!
!!    REAL*8 GAMA,XNP,XSP,CXSTAR,CYSTAR,CX,CY,TEMP1,TEMP2,TEMP3,TEMP4
      REAL*8 GAMA,XNP(nl),XSP(nl),CXSTAR,CYSTAR,CX,CY,TEMP1,TEMP2,TEMP3,TEMP4
!
      REAL*8  ZERO,HALF,FOURTH,EPSM
      REAL*8  DSNP,DSSP,DTDLN(NY),DTDLT(NY),GC(NY)
      DATA ZERO,HALF,FOURTH,EPSM/ 0.0D0,0.5D0,0.25D0,1.0D-80/
*     DATA ZERO,HALF,FOURTH,EPSM/ 0.0D0,0.5D0,0.25D0,1.0E-6/
      INTEGER I,J,K
      integer begj,endj

      begj = 2
      endj = ny-1
!
       DO K=1,NL
       DO J=begj,endj
       DO I=1,NX
         QMIN(I,J,K)=1.0E15
         QMAX(I,J,K)=-1.0E15
       ENDDO
       ENDDO
       ENDDO

!-H ---
      if (inc_pole) then
         DO I = 1 ,NX
            H(I,Jpole) = ZERO
         ENDDO
      endif

      do j = jbeg, jend
         DO I = IB,IE
            H(I,J) = GC(J) * P(I,J)
         ENDDO
            H(1 ,J)  = H(IE,J)
            H(NX,J)  = H(IB,J)
      enddo

      call mpi_isend( h(1,2), nx,mpi_double_precision,itop,1,mpicom,isend11,ierr )
      call mpi_irecv( h(1,ny),nx,mpi_double_precision,ibot,1,mpicom,irecv11,ierr )

!-HU,CU --

      if (inc_pole) then
         DO I = 1 ,NX
            HU(I,Jpole) = ZERO
            CU(I,Jpole) = ZERO
         ENDDO
      endif

      do j=jbeg, jend
          DO I = IB,IE
            HU(I,J)  = HALF*(H(I,J)+H(I-1,J))
          ENDDO
          HU(1 ,J) = HU(IE,J)
          HU(NX,J) = HU(IB,J)
          DO I = 1 ,NX
            CU(I,J) = DTDLN(J) * HU(I,J)
          ENDDO
      enddo

!-HV,CV --
      call mpi_wait( isend11,istatus,ierr )
      call mpi_wait( irecv11,istatus,ierr )

      DO J = 2 ,JEnd
        DO I = IB,IE
          HV(I,J)  = HALF*(H(I,J)+H(I,J+1))
        ENDDO
        HV(1 ,J) = HV(IE,J)
        HV(NX,J) = HV(IB,J)
        DO I = 1 ,NX
          CV(I,J) = DTDLT(J) * HV(I,J)
        ENDDO
      ENDDO
c----
      DO K=1,NL
      DO J=begj,endj
      DO I=1,NX
        QH(I,J,k)=Q(I,J,K)
      ENDDO
      ENDDO
      ENDDO

      ii=1
      do k=1,nl
       do i=1,nx
          workst1(ii) = qh(i,2   ,k)
          worksb1(ii) = qh(i,ny-1,k)
          ii=ii+1
       enddo
      enddo

      call mpi_isend(workst1,nx*nl,mpi_double_precision,itop,2,mpicom,isend11,ierr)
      call mpi_irecv(workrb1,nx*nl,mpi_double_precision,ibot,2,mpicom,irecv11,ierr)
      call mpi_isend(worksb1,nx*nl,mpi_double_precision,ibot,3,mpicom,isend12,ierr)
      call mpi_irecv(workrt1,nx*nl,mpi_double_precision,itop,3,mpicom,irecv12,ierr)

C----
      DO K=1,NL
       DO J=JBeg,JEnd
        DO I=IB,IE
	  UU(I,J,k)=CU(I,J)*U(I,J,K)
        ENDDO
 	UU(1 ,J,k)=UU(IE,J,k)
        UU(NX,J,k)=UU(IB,J,k)
       ENDDO
      ENDDO
C----
      DO K=1,NL
       DO J=2,JEnd
        DO I=IB,IE
	  VV(I,J,k)=CV(I,J)*V(I,J,K)
        ENDDO
	VV(1 ,J,k)=VV(IE,J,k)
	VV(NX,J,k)=VV(IB,J,k)
       ENDDO
      ENDDO

      ii=1
       do i=1,nx
          worksb2(ii)=hv(i,ny-1)
          ii=ii+1
       enddo
      do k=1,nl
       do i=1,nx
          worksb2(ii)=vv(i,ny-1,k)
          ii=ii+1
       enddo
      enddo

      call mpi_isend(worksb2,nx*(nl+1),mpi_double_precision,ibot,4,mpicom,isend22,ierr)
      call mpi_irecv(workrt2,nx*(nl+1),mpi_double_precision,itop,4,mpicom,irecv22,ierr)

C----
      DO K=1,NL
       DO J=JBeg,JEnd
	DO I=IB,IE
	  FX(I,J,k)=HALF*UU(I,J,k)*(QH(I,J,k)+QH(I-1,J,k))
     $             -HALF*UU(I,J,k)*UU(I,J,k)*(QH(I,J,k)-QH(I-1,J,k))/HU(I,J)
	ENDDO
      	FX(NX,J,k)=FX(IB,J,k)
       ENDDO
      ENDDO
C----
      call mpi_wait( isend11,istatus,ierr )
      call mpi_wait( irecv11,istatus,ierr )
      call mpi_wait( isend12,istatus,ierr )
      call mpi_wait( irecv12,istatus,ierr )

      ii=1
      do k=1,nl
       do i=1,nx
          qh(i,ny,k) = workrb1(ii)
          qh(i,1 ,k) = workrt1(ii)
          ii=ii+1
       enddo
      enddo

      DO K=1,NL
       DO J=2,JEnd
        DO I=IB,IE
	  FY(I,J,k)=HALF*VV(I,J,k)*(QH(I,J+1,k)+QH(I,J,k))
     $             -HALF*VV(I,J,k)*VV(I,J,k)*(QH(I,J+1,k)-QH(I,J,k))/HV(I,J)
	ENDDO
       ENDDO
      ENDDO
C----
      ii=1
      do k=1,nl
       do i=1,nx
          worksb1(ii)=fy(i,ny-1,k)
          ii=ii+1
       enddo
      enddo

      call mpi_isend(worksb1,nx*nl,mpi_double_precision,ibot,5,mpicom,isend12,ierr)
      call mpi_irecv(workrt1,nx*nl,mpi_double_precision,itop,5,mpicom,irecv12,ierr)
!--
      call mpi_wait( isend22,istatus,ierr )
      call mpi_wait( irecv22,istatus,ierr )

      ii=1
       do i=1,nx
          hv(i,1)=workrt2(ii)
          ii=ii+1
       enddo
      do k=1,nl
       do i=1,nx
          vv(i,1,k)=workrt2(ii)
          ii=ii+1
       enddo
      enddo
!--
      call mpi_wait( isend12,istatus,ierr )
      call mpi_wait( irecv12,istatus,ierr )

      ii=1
      do k=1,nl
       do i=1,nx
          fy(i,1,k)=workrt1(ii)
          ii=ii+1
       enddo
      enddo

      DO K=1,NL
       DO J=JBeg,JEnd
	DO I=IB,IE
          TEMP1=ABS(UU(I,J,k)/HU(I,J))*(1-ABS(UU(I,J,k)/HU(I,J)))
          TEMP2=ABS(UU(I+1,J,k)/HU(I+1,J))*(1-ABS(UU(I+1,J,k)/HU(I+1,J)))
          TEMP3=ABS(VV(I,J-1,k)/HV(I,J-1))*(1-ABS(VV(I,J-1,k)/HV(I,J-1)))
	  TEMP4=ABS(VV(I,J,k)/HV(I,J))*(1-ABS(VV(I,J,k)/HV(I,J)))
	  GAMA=MAX(TEMP1,TEMP2,TEMP3,TEMP4)
          BETA(I,J,k)=2.0D0/(2.0D0-2.0D0*GAMA)
          QHSTAR(I,J,k)=QH(I,J,k)-BETA(I,J,k)*(FX(I+1,J,k)
     $                    -FX(I,J,k)+FY(I,J,k)-FY(I,J-1,k))/H(I,J)
        ENDDO
       ENDDO
      ENDDO
C------
      if (nprocs.ge.32) then

         if (myrank.eq.1) then
             call mpi_send( h(1,ny-1),nx,mpi_double_precision,0,61,mpicom,ierr)
             ii=1
             do k=1,nl
              do i=1,nx
                 worksb1(ii)=beta(i,ny-1,k)
                 ii=ii+1
              enddo
             enddo
             call mpi_isend(worksb1,nx*nl,mpi_double_precision,0,62,mpicom,isend12,ierr)
             call mpi_wait(isend12,istatus,ierr)
         endif

         if (myrank.eq.0 ) then
             call mpi_recv( h(1,1   ),nx,mpi_double_precision,1,61,mpicom,ierr)
             call mpi_irecv(workrt1,nx*nl,mpi_double_precision,1,62,mpicom,irecv12,ierr)
             call mpi_wait(irecv12,istatus,ierr)
             ii=1
             do k=1,nl
              do i=1,nx
                 beta(i,1,k)=workrt1(ii)
                 ii=ii+1
              enddo
             enddo
         endif

         if (myrank.eq.nprocs-2) then
             ii=1
             do k=1,nl
              do i=1,nx
                 workst1(ii)=beta(i,2,k)
                 ii=ii+1
              enddo
             enddo
             call mpi_isend(workst1,nx*nl,mpi_double_precision,nprocs-1,
     _                                 62,mpicom,isend12,ierr)
             call mpi_wait(isend12,istatus,ierr)
         endif

         if (myrank.eq.nprocs-1 ) then
             call mpi_irecv(workrb1,nx*nl,mpi_double_precision,nprocs-2,
     _                                 62,mpicom,irecv12,ierr)
             call mpi_wait(irecv12,istatus,ierr)
             ii=1
             do k=1,nl
              do i=1,nx
                 beta(i,ny,k)=workrb1(ii)
                 ii=ii+1
              enddo
             enddo
         endif

      endif
!--------------
      if (myrank.eq.nprocs-1) then
       DO K=1,NL
         DO 440 I = IB,IE
            C(I,1,k) =BETA(I,jpole+1,k)*FY(I,jpole,k)/H(I,jpole+1)
440      CONTINUE
            XNP(k) = ZERO
         DO 450 I = IB,IE
            XNP(k)   = XNP(k) + C(I,1,k)
450      CONTINUE
            XNP(k)   = QH(IB,jpole,k) - XNP(k)*DSNP
         DO 460 I = IB,IE
            QHSTAR(I,jpole,k)   = XNP(k)
460      CONTINUE
       ENDDO
      endif

      if (myrank.eq.0) then
       DO K=1,NL
         DO 441 I = IB,IE
            C(I,2,k) =BETA(I,Jpole-1,k)*FY(I,jpole-1,k)/H(I,jpole-1)
441      CONTINUE
            XSP(k)   = ZERO
         DO 451 I = IB,IE
            XSP(k)   = XSP(k) + C(I,2,k)
451      CONTINUE
            XSP(k)   = QH(IB,jpole,k)+ XSP(k)*DSSP
         DO 461 I = IB,IE
            QHSTAR(I,jpole,k)   = XSP(k)
461      CONTINUE
       ENDDO
      endif
C----
      DO K=1,NL
      DO J=begj,endj
	QHSTAR(1,J,k)=QHSTAR(IE,J,k)
	QHSTAR(NX,J,k)=QHSTAR(IB,J,k)
      ENDDO
      ENDDO
C----
      if (myrank.eq.nprocs-1) then
         do k=1,nl
          do i=ib,ie
	   QMIN(I,Jpole,K)=MIN(QH(I,Jpole,k),QH(I,Jpole+1,k),QMIN(I,Jpole,K))
	   QMAX(I,Jpole,K)=MAX(QH(I,Jpole,k),QH(I,Jpole+1,k),QMAX(I,Jpole,K))
           A(I,Jpole,k)=(QHSTAR(I,Jpole,k)-QMAX(I,Jpole,K))*(QHSTAR(I,Jpole,k)-QMIN(I,Jpole,K))
          enddo
	   A(1,Jpole,k)=A(IE,Jpole,k)
	   A(NX,Jpole,k)=A(IB,Jpole,k)
         enddo
      endif

      if (myrank.eq.0) then
         do k=1,nl
          do i=ib,ie
	    QMIN(I,Jpole,K)=MIN(QH(I,Jpole,k),QH(I,Jpole-1,k),QMIN(I,Jpole,K))
	    QMAX(I,Jpole,K)=MAX(QH(I,Jpole,k),QH(I,Jpole-1,k),QMAX(I,Jpole,K))
            A(I,Jpole,k)=(QHSTAR(I,Jpole,k)-QMAX(I,Jpole,K))*(QHSTAR(I,Jpole,k)-QMIN(I,Jpole,K))
          enddo
	    A(1,Jpole,k)=A(IE,Jpole,k)
	    A(NX,Jpole,k)=A(IB,Jpole,k)
         enddo
      endif

      DO K=1,NL
      DO J=jbeg,jend
        DO I=IB,IE
	  QMIN(I,J,K)=MIN(QH(I,J,k),QH(I+1,J,k),QH(I-1,J,k),
     $                    QH(I,J+1,k),QH(I,J-1,k),QMIN(I,J,K))
          QMAX(I,J,K)=MAX(QH(I,J,k),QH(I+1,J,k),QH(I-1,J,k),
     $                    QH(I,J+1,k),QH(I,J-1,k),QMAX(I,J,K))
          A(I,J,k)=(QHSTAR(I,J,k)-QMAX(I,J,K))*(QHSTAR(I,J,k)-QMIN(I,J,K))
        ENDDO
	A(1,J,k)=A(IE,J,k)
	A(NX,J,k)=A(IB,J,k)
      ENDDO
      ENDDO

      ii=1
      do k=1,nl
       do i=1,nx
          workst1(ii)=a(i,2,k)
          ii=ii+1
       enddo
      enddo

      call mpi_isend(workst1,nx*nl,mpi_double_precision,itop,7,mpicom,isend11,ierr)
      call mpi_irecv(workrb1,nx*nl,mpi_double_precision,ibot,7,mpicom,irecv11,ierr)

C----
      DO K=1,NL
      DO J=JBeg,JEnd
      DO I=IB,IE
	TEMP1=(ABS(A(I-1,J,k))+A(I-1,J,k))/(ABS(A(I-1,J,k))+EPSM)
	TEMP2=(ABS(A(I,J,k))+A(I,J,k))/(ABS(A(I,J,k))+EPSM)
	TEMP3=(ABS(A(I-1,J,k))+A(I-1,J,k))*(ABS(A(I,J,k))+A(I,J,k))
	TEMP4=ABS(A(I-1,J,k))*ABS(A(I,J,k))+EPSM
	CXSTAR=HALF*(TEMP1+TEMP2)-FOURTH*TEMP3/TEMP4
        CX=CXSTAR+(1-CXSTAR)*ABS(UU(I,J,k)/HU(I,J))
        USTAR(I,J,k)=CX*UU(I,J,k)
      ENDDO
      ENDDO
      ENDDO
C----
      call mpi_wait( isend11,istatus,ierr )
      call mpi_wait( irecv11,istatus,ierr )

      ii=1
      do k=1,nl
       do i=1,nx
          a(i,ny,k)=workrb1(ii)
          ii=ii+1
       enddo
      enddo

      DO K=1,NL
      DO J=2,JEnd
      DO I=IB,IE
	TEMP1=(ABS(A(I,J,k))+A(I,J,k))/(ABS(A(I,J,k))+EPSM)
	TEMP2=(ABS(A(I,J+1,k))+A(I,J+1,k))/(ABS(A(I,J+1,k))+EPSM)
	TEMP3=(ABS(A(I,J,k))+A(I,J,k))*(ABS(A(I,J+1,k))+A(I,J+1,k))
	TEMP4=ABS(A(I,J,k))*ABS(A(I,J+1,k))+EPSM
	CYSTAR=HALF*(TEMP1+TEMP2)-FOURTH*TEMP3/TEMP4
        CY=CYSTAR+(1-CYSTAR)*ABS(VV(I,J,k)/HV(I,J))
        VSTAR(I,J,k)=CY*VV(I,J,k)
      ENDDO
      ENDDO
      ENDDO
C----
      DO K=1,NL
      DO J=2,JEnd
      DO I=IB,IE
	  FY(I,J,k)=HALF*VV(I,J,k)*(QH(I,J+1,k)+QH(I,J,k))
     $             -HALF*ABS(VSTAR(I,J,k))*(QH(I,J+1,k)-QH(I,J,k))
      ENDDO
      ENDDO
      ENDDO

      ii=1
      do k=1,nl
       do i=1,nx
          worksb1(ii)=fy(i,ny-1,k)
          ii=ii+1
       enddo
      enddo

      call mpi_isend(worksb1,nx*nl,mpi_double_precision,ibot,8,mpicom,isend12,ierr)
      call mpi_irecv(workrt1,nx*nl,mpi_double_precision,itop,8,mpicom,irecv12,ierr)
C----
      DO K=1,NL
       DO J=JBeg,JEnd
	DO I=IB,IE
	  FX(I,J,k)=HALF*UU(I,J,k)*(QH(I,J,k)+QH(I-1,J,k))
     $           -HALF*ABS(USTAR(I,J,k))*(QH(I,J,k)-QH(I-1,J,k))
	ENDDO
	FX(NX,J,k)=FX(IB,J,k)
       ENDDO
      ENDDO
!--
      call mpi_wait( isend12,istatus,ierr )
      call mpi_wait( irecv12,istatus,ierr )

      ii=1
      do k=1,nl
       do i=1,nx
          fy(i,1,k)=workrt1(ii)
          ii=ii+1
       enddo
      enddo

C----
      DO K=1,NL
       DO J=JBeg,JEnd
        DO I=IB,IE
          QH(I,J,k)=QH(I,J,k)-(FX(I+1,J,k)-FX(I,J,k)
     $                        +FY(I,J,k)-FY(I,J-1,k))/H(I,J)
        ENDDO
       ENDDO
      ENDDO
C-----
       if (nprocs.ge.32) then
         if (myrank.eq.1) then
            ii=1
            do k=1,nl
             do i=1,nx
                worksb1(ii)=fy(i,ny-1,k)
                ii=ii+1
             enddo
            enddo
         call mpi_isend(worksb1,nx*nl,mpi_double_precision,0,9,mpicom,isend12,ierr)
         call mpi_wait(isend12,istatus,ierr)
         endif

         if (myrank.eq.0) then
         call mpi_irecv(workrt1,nx*nl,mpi_double_precision,1,9,mpicom,irecv12,ierr)
         call mpi_wait(irecv12,istatus,ierr)
            ii=1
            do k=1,nl
             do i=1,nx
                fy(i,1,k)=workrt1(ii)
                ii=ii+1
             enddo
            enddo
         endif

       endif
!---
      if (myrank.eq.nprocs-1) then
      DO K=1,NL
      DO 540 I = IB,IE
      C(I,1,k) = FY(I,jpole,k) / H(I,jpole+1)
540   CONTINUE
      XNP(k)   = ZERO
      DO 550 I = IB,IE
      XNP(k)   = XNP(k) + C(I,1,k)
550   CONTINUE
      XNP(k)   = QH(IB,jpole,k) - XNP(k)*DSNP
      DO 560 I = IB,IE
      QH(I,jpole,k)   = XNP(k)
560   CONTINUE
      ENDDO
      endif

      if (myrank.eq.0) then
      DO K=1,NL
      DO 541 I = IB,IE
      C(I,2,k) = FY(I,jpole-1,k) / H(I,jpole-1)
541   CONTINUE
      XSP(k)   = ZERO
      DO 551 I = IB,IE
      XSP(k)   = XSP(k)     + C(I,2,k)
551   CONTINUE
      XSP(k)   = QH(IB,jpole,k) + XSP(k)*DSSP
      DO 561 I = IB,IE
      QH(I,jpole,k)   = XSP(k)
561   CONTINUE
      ENDDO
      endif
C----
      DO K=1,NL
      DO J=begj,endj
        QH(1,J,k)=QH(IE,J,k)
        QH(NX,J,k)=QH(IB,J,k)
      ENDDO
      ENDDO
C----

      ii=1
      do k=1,nl
       do i=1,nx
          workst1(ii)=qh(i,2   ,k)
          worksb1(ii)=qh(i,ny-1,k)
          ii=ii+1
       enddo
      enddo

        call mpi_isend(workst1,nx*nl,mpi_double_precision,itop,10,mpicom,isend11,ierr)
        call mpi_isend(worksb1,nx*nl,mpi_double_precision,ibot,10,mpicom,isend12,ierr)
        call mpi_irecv(workrb1,nx*nl,mpi_double_precision,ibot,10,mpicom,irecv11,ierr)
        call mpi_irecv(workrt1,nx*nl,mpi_double_precision,itop,10,mpicom,irecv12,ierr)
!--
      DO K=1,NL
      DO J=begj,endj
      DO I=1,NX
	Q(I,J,K)=QH(I,J,k)
      ENDDO
      ENDDO
      ENDDO

!---
        call mpi_wait(isend11,istatus,ierr)
        call mpi_wait(isend12,istatus,ierr)
        call mpi_wait(irecv11,istatus,ierr)
        call mpi_wait(irecv12,istatus,ierr)

      ii=1
      do k=1,nl
       do i=1,nx
          qh(i,ny,k)=workrb1(ii)
          qh(i,1 ,k)=workrt1(ii)
          ii=ii+1
       enddo
      enddo

C
C*********BEGIN CROSS ITEM************
      DO K=1,NL
      DO J=JBeg,JEnd
      DO I=IB,IE
	TEMP1=0.5*(UU(I,J,k)+UU(I+1,J,k))
	TEMP2=0.5*(VV(I,J-1,k)+VV(I,J,k))
	TEMP3=0.25*(QH(I+1,J+1,k)+QH(I-1,J-1,k)-QH(I+1,J-1,k)-QH(I-1,J+1,k))
	Q(I,J,K)=QH(I,J,k)+TEMP1*TEMP2*TEMP3/(H(I,J)*H(I,J))
      ENDDO
      ENDDO
      ENDDO
C*********END CROSS ITEM*************
C
      DO K=1,NL
      DO J=begj,endj
        Q(1,J,K)=Q(IE,J,K)
        Q(NX,J,K)=Q(IB,J,K)
      ENDDO
      ENDDO

!- check ---------------------------------------------------------
!
!      write(filename,14) 'tspas-p-',myrank,'.out'
!14    format(a8,i1,a4)
!
!      open (10,file=trim(filename))
!
!      write(10,*) 'qqqqqqqqqqqqqqqqqqqqq'
!      do j=1,ny
!        write(10,11) j,q(1,j,10),q(2,j,10)
!      enddo
!
!11    format(1x,i5,2e30.20)
!      close (10)
!
!      call mpi_finalize(j)
!      stop'tspas'
!--------------------------------------------------------------

C
C
      RETURN
	END


