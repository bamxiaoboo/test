# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/MPDATA.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/MPDATA.F"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/MPDATA.F" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/MPDATA.F" 2

!!(wh 2003.11.11)
!!---------------------



       SUBROUTINE MPDATA(Q,U,V,P,DSNP,DSSP,GC,DTDLT,DTDLN
     _                 ,EP,NONOS,IORD,ISOR,IP)
C     **************************
C     **************************
C
C     PERFORM 2-D ADVECTION IN GLOBAL SPHERICAL GEOMETRY
C             WITH THE UNIFORM LAT-LON C-GRID MESH
C

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
# 23 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/MPDATA.F" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/PARADD" 1
      INTEGER IB,IE,JB,JE,KE,NM
      PARAMETER ( IB=2,IE=NX-1,JB=2,JE=NY-1,KE=NL+2,NM=NL-1 )
# 24 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/MPDATA.F" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/commpi.h" 1

        
      integer nprocs, myrank, ierr, itop, ibot, jbeg, jend, jpole
      logical inc_pole

      common/commpi/ nprocs, myrank, itop, ibot, jbeg, jend, jpole, inc_pole
# 25 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/MPDATA.F" 2

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
# 26 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/MPDATA.F" 2

      character*50 filename

      integer isend1,irecv1
      integer isend2,irecv2
      integer istatus(mpi_status_size)

      real*8 workst(nx*nl),workrb(nx*nl)
      real*8 worksb(nx*nl),workrt(nx*nl)
      integer ii

      integer endj

!
      REAL*8  Q(NX,NY,NL),U(NX,NY,NL),V(NX,NY,NL)
     &       ,P(NX,NY)
      REAL*8  VU(NX,NY,nl),VV(NX,NY,nl),FU(NX,NY,nl),FV(NX,NY,nl)
     &       ,CU(NX,NY),CV(NX,NY),HU(NX,NY),HV(NX,NY)
     &       ,UU(NX,NY,nl),UV(NX,NY,nl),BU(NX,NY,nl),BD(NX,NY,nl)
     &       ,X (NX,NY+1,nl),H (NX,NY),XM(NX,NY,nl),XN(NX,NY,nl)
     &       ,C (NX,2 ,nl)
C
      REAL*8  EP,DSNP,DSSP,DTDLN(NY),DTDLT(NY),GC(NY)
      REAL*8  ZERO,HALF,ONE
      DATA ZERO,HALF,ONE / 0.0D0,0.5D0,1.0D0 /
      INTEGER I,J,K,IO
      INTEGER NONOS,IORD,ISOR,IP(NX)
      REAL*8  XNP(nl),XSP(nl)
C     =========================================================
      REAL*8  A,VDYF,R,X1,X2,VCOR,B,YY,VC31,VC32,PP,Y,PN
      VDYF(R,A,X1,X2)= (ABS(A)-A**2/R)*(X2-X1)/(X2+X1+EP)
      VCOR(R,A,B,YY) =-0.125*A*B*YY/R
      VC31(R,A,YY)   =-(1.0-3.0*ABS(A)/R+2.0*(A/R)**2)*A*YY/3.0
      VC32(R,A,B,YY) = 0.25*B/R*(ABS(A)-2.0*A**2/R)*YY
      PP(Y)          = MAX(0.0,Y)
      PN(Y)          =-MIN(0.0,Y)
C     =========================================================
C
C     GET WORKSHOPS INDEPENDENT OF VERTICAL COORDINATE
!-H ---
        do j=jbeg,jend
          DO I = IB,IE
            H(I,J) = GC(J) * P(I,J)
          ENDDO
          H(1 ,J)  = H(IE,J)
          H(NX,J)  = H(IB,J)
        enddo               !---------------
        if (inc_pole) then  ! for the poles
          DO I = 1 ,NX
            H(I,jpole) = ZERO
          ENDDO
        ENDIF

        call mpi_isend( h(1,2), nx,mpi_double_precision,itop,1,mpicom,isend1,ierr )
        call mpi_irecv( h(1,ny),nx,mpi_double_precision,ibot,1,mpicom,irecv1,ierr )

!-HU,CU --
        do j=jbeg,jend
          DO I = IB,IE
            HU(I,J)  = HALF*(H(I-1,J)+H(I,J))
          ENDDO
          HU(1 ,J) = HU(IE,J)
          HU(NX,J) = HU(IB,J)
          DO I = 1 ,NX
            CU(I,J) = DTDLN(J) * HU(I,J)
          ENDDO
        enddo                  !---------------
        if (inc_pole) then     ! for the poles
          DO I = 1 ,NX
            HU(I,jpole) = ZERO
            CU(I,jpole) = ZERO
          ENDDO
        ENDIF

!-HV,CV --
        call mpi_wait( isend1,istatus,ierr )
        call mpi_wait( irecv1,istatus,ierr )

        DO J = 2,jend
         DO I = IB,IE
            HV(I,J)  = HALF*(H(I,J)+H(I,J+1))
         ENDDO
            HV(1 ,J) = HV(IE,J)
            HV(NX,J) = HV(IB,J)
         DO I = 1 ,NX
            CV(I,J) = DTDLT(J) * HV(I,J)
         ENDDO
        ENDDO

!-------------------------------------------------------------
C
C     START VERTICAL LAYER DO LOOP
C$DOACROSS LOCAL(K,J,I,UU,UV,X,XM,XN,FU,FV,C,XNP,XSP,VU,VV,BU,BD,IO)

!-X ----
        do 250 k = 1 ,nl
        DO 250 J = 2 ,NY-1
        DO 250 I = 1 ,NX
           X (I,J,k)  = Q (I,J,K)
250     CONTINUE

        ii=1
        do k=1,nl
         do i=1,nx
            workst(ii)=x(i,2,   k)
            worksb(ii)=x(i,ny-1,k)
            ii=ii+1
         enddo
        enddo

        call mpi_isend(workst,nx*nl,mpi_double_precision,itop,2,mpicom,isend1,ierr)
        call mpi_isend(worksb,nx*nl,mpi_double_precision,ibot,3,mpicom,isend2,ierr)
        call mpi_irecv(workrb,nx*nl,mpi_double_precision,ibot,2,mpicom,irecv1,ierr)
        call mpi_irecv(workrt,nx*nl,mpi_double_precision,itop,3,mpicom,irecv2,ierr)
!-------
        if (inc_pole) then
          do k=1,nl
          DO I = 1 ,NX
            VU(I,Jpole,k) = ZERO
            FU(I,Jpole,k) = ZERO
          ENDDO
          enddo
        endif
!-------
C
C     CALCULATE COURANT NUMBERS MULTIPLIED BY H
!---
       do k=1,nl
        do j=jbeg,jend
         do i=ib,ie
            UU(I,J,k)  = CU(I,J) * U(I,J,K)
         enddo
            UU(1 ,J,k) = UU(IE,J,k)
            UU(NX,J,k) = UU(IB,J,k)
        enddo
       enddo
!---
       do k=1,nl
        do j=2,jend
         do i=ib,ie
            UV(I,J,k)  = CV(I,J) * V(I,J,K)
         enddo
            UV(1 ,J,k) = UV(IE,J,k)
            UV(NX,J,k) = UV(IB,J,k)
        enddo
       enddo
!---
C
       do 240 k = 1,nl
        DO 240 J = jbeg,jend
         DO 240 I = 1 ,NX
            VU(I,J,k)  = UU(I,J,k)
240    CONTINUE
!---
       do 245 k =1,nl
        DO 245 J = 2 ,jend
         DO 245 I = 1 ,NX
            VV(I,J,k)  = UV(I,J,k)
245    CONTINUE
!-----------------------------------------------------

C     PREPARE FOR NON-OSSCILATORY OPTION

        call mpi_wait( isend1,istatus,ierr )
        call mpi_wait( irecv1,istatus,ierr )
        call mpi_wait( isend2,istatus,ierr )
        call mpi_wait( irecv2,istatus,ierr )

        ii=1
        do k=1,nl
         do i=1,nx
            x(i,1, k)=workrt(ii)
            x(i,ny,k)=workrb(ii)
            ii=ii+1
         enddo
        enddo

      IF( NONOS.EQ.1 ) THEN
        do 300 k = 1,nl
        DO 300 J = JBeg,JEnd
        DO 300 I = IB,IE
          XM(I,J,k)  = MAX( X(I-1,J,k),X(I,J,k),X(I+1,J,k),X(I,J-1,k),X(I,J+1,k) )
          XN(I,J,k)  = MIN( X(I-1,J,k),X(I,J,k),X(I+1,J,k),X(I,J-1,k),X(I,J+1,k) )
300     CONTINUE
      ENDIF
C
      DO 700 IO= 1 ,IORD
C     ++++++++++++++++++++++++++++++++
C     PREDICTOR STEP : UPSTREAM SCHEME
C     ++++++++++++++++++++++++++++++++
      do 410 k = 1,nl
      DO 410 J = JBeg,JEnd
       DO 400 I = IB,IE
        IF(VU(I,J,k).GE.0.0E0) THEN
         FU(I,J,k)= X(I-1,J,k)*VU(I,J,k)
        ELSE
         FU(I,J,k)= X(I,J,k)*VU(I,J,k)
        ENDIF
400    CONTINUE
       FU(NX,J,k) = FU(IB,J,k)
410   CONTINUE

      do 420 k = 1, nl
      DO 420 J = 2 ,JEnd
      DO 420 I = IB,IE
        IF(VV(I,J,k).GE.0.0E0) THEN
          FV(I,J,k)= X(I,J,k)*VV(I,J,k)
        ELSE
          FV(I,J,k)= X(I,J+1,k)*VV(I,J,k)
        ENDIF
420   CONTINUE

      ii=1
      do k=1,nl
       do i=1,nx
          worksb(ii)=fv(i,ny-1,k)
          ii=ii+1
       enddo
      enddo
        call mpi_isend(worksb,nx*nl,mpi_double_precision,ibot,4,mpicom,isend1,ierr)
        call mpi_irecv(workrt,nx*nl,mpi_double_precision,itop,4,mpicom,irecv1,ierr)
        call mpi_wait(isend1,istatus,ierr)
        call mpi_wait(irecv1,istatus,ierr)
      ii=1
      do k=1,nl
       do i=1,nx
          fv(i,1,k)=workrt(ii)
          ii=ii+1
       enddo
      enddo

      do 430 k = 1,nl
      DO 430 J = JBeg,JEnd
      DO 430 I = IB,IE
      X(I,J,k)   = X(I,J,k)-(FU(I+1,J,k)-FU(I,J,k)+FV(I,J,k)-FV(I,J-1,k))/H(I,J)
430   CONTINUE


C     B.C. BY THE SPHERICAL CYCLICITY & MASS CONSERVATION RESTRICT

      if (nprocs.ge.32) then
         if (myrank.eq.1)
     _     call mpi_send( h(1,ny-1),nx,mpi_double_precision,0,5,mpicom,ierr)
         if (myrank.eq.0)
     _     call mpi_recv( h(1,1),   nx,mpi_double_precision,1,5,mpicom,ierr)
      endif

      if (myrank.eq.(nprocs-1)) then
         do  k = 1,nl
          XNP(k)      = ZERO
          do I = IB,IE
             C(I,1,k)= FV(I,jpole,k) / H(I,jpole+1)
             XNP(k)  = XNP(k) + C(I,1,k)
          enddo
             XNP(k)      = X(IB,jpole,k) - XNP(k)*DSNP
          do i=ib,ie
             X(I,jpole,k)   = XNP(k)
          enddo
         enddo
      endif

      if (myrank.eq.0) then
         do k=1,nl
          XSP(k)      = ZERO
          do I = IB,IE
             C(I,2,k) = FV(I,jpole-1,k) / H(I,jpole-1)
             XSP(k)   = XSP(k) + C(I,2,k)
          enddo
             XSP(k)   = X(IB,jpole,k) + XSP(k)*DSSP
          do i=ib,ie
             X(I,jpole,k)  = XSP(k)
          enddo
         enddo
      endif

        do 470 k = 1,nl
        DO 470 J = 2 ,NY-1
        X(1 ,J,k)  = X(IE,J,k)
        X(NX,J,k)  = X(IB,J,k)
470     CONTINUE
C
      IF( IO.EQ.IORD ) GOTO 700
C     ++++++++++++++++++++++++++++++++++++++
C     CORRECTOR STEP : ANTI-DIFFUSION SCHEME
C     ++++++++++++++++++++++++++++++++++++++

        ii=1
        do k=1,nl
         do i=1,nx
            workst(ii)=x(i,2,   k)
            worksb(ii)=x(i,ny-1,k)
            ii=ii+1
         enddo
        enddo
        call mpi_isend(workst,nx*nl,mpi_double_precision,itop,6,mpicom,isend1,ierr)
        call mpi_isend(worksb,nx*nl,mpi_double_precision,ibot,7,mpicom,isend2,ierr)
        call mpi_irecv(workrb,nx*nl,mpi_double_precision,ibot,6,mpicom,irecv1,ierr)
        call mpi_irecv(workrt,nx*nl,mpi_double_precision,itop,7,mpicom,irecv2,ierr)
!-----

      do 500 k = 1,nl
      DO 500 J = JBeg,JEnd
      DO 500 I = 1 ,NX
      FU(I,J,k)  = VU(I,J,k)
500   CONTINUE

      do 505 k = 1,nl
      DO 505 J = 2 ,JEnd
      DO 505 I = 1 ,NX
      FV(I,J,k)  = VV(I,J,k)
505   CONTINUE
!-----
        call mpi_wait( isend1,istatus,ierr )
        call mpi_wait( irecv1,istatus,ierr )
        call mpi_wait( isend2,istatus,ierr )
        call mpi_wait( irecv2,istatus,ierr )

        ii=1
        do k=1,nl
         do i=1,nx
            x(i,1, k)=workrt(ii)
            x(i,ny,k)=workrb(ii)
            ii=ii+1
         enddo
        enddo
!-----

C     CALCULATE THE  PSEUDO VELOCITIES
C               LONGITUDINAL DIRECTION

        ii=1
        do k=1,nl
         do i=1,nx
            workst(ii)=fu(i,2,   k)
            worksb(ii)=fv(i,ny-1,k)
            ii=ii+1
         enddo
        enddo

        call mpi_isend(workst,nx*nl,mpi_double_precision,itop,8,mpicom,isend1,ierr)
        call mpi_isend(worksb,nx*nl,mpi_double_precision,ibot,9,mpicom,isend2,ierr)
        call mpi_irecv(workrb,nx*nl,mpi_double_precision,ibot,8,mpicom,irecv1,ierr)
        call mpi_irecv(workrt,nx*nl,mpi_double_precision,itop,9,mpicom,irecv2,ierr)

        call mpi_wait( isend1,istatus,ierr )
        call mpi_wait( irecv1,istatus,ierr )
        call mpi_wait( isend2,istatus,ierr )
        call mpi_wait( irecv2,istatus,ierr )

        ii=1
        do k=1,nl
         do i=1,nx
            fv(i,1, k)=workrt(ii)
            fu(i,ny,k)=workrb(ii)
            ii=ii+1
         enddo
        enddo

      do 510 k = 1,nl
      DO 510 J = JBeg,JEnd
      DO 510 I = IB,IE
      VU(I,J,k)  = VDYF( HU(I,J),FU(I,J,k),X(I-1,J,k),X(I,J,k) )
     &         + VCOR( HU(I,J),FU(I,J,k)
     &         ,      (FV(I-1,J-1,k)+FV(I-1,J,k)+FV(I,J,k)+FV(I,J-1,k))
     &         ,      (X(I-1,J+1,k)+X(I,J+1,k)-X(I-1,J-1,k)-X(I,J-1,k))
     &         /   (EP+X(I-1,J+1,k)+X(I,J+1,k)+X(I-1,J-1,k)+X(I,J-1,k)) )
510   CONTINUE
C               LATITUDINAL  DIRECTION

      do 520 k = 1,nl
      DO 520 J = 2 ,JEnd
      DO 520 I = IB,IE
      VV(I,J,k)  = VDYF( HV(I,J),FV(I,J,k),X(I,J,k),X(I,J+1,k) )
     &         + VCOR( HV(I,J),FV(I,J,k)
     &         ,      (FU(I,J,k)+FU(I,J+1,k)+FU(I+1,J,k)+FU(I+1,J+1,k))
     &         ,      (X(I+1,J,k)+X(I+1,J+1,k)-X(I-1,J,k)-X(I-1,J+1,k))
     &         /   (EP+X(I+1,J,k)+X(I+1,J+1,k)+X(I-1,J,k)+X(I-1,J+1,k)) )
520   CONTINUE

!---------------------------------------------------------------
C     ADD THE THIRD ORDER CORRECTION IF REQUESTED
      IF( ISOR.EQ.3 ) THEN
C               LONGITUDINAL DIRECTION

      ii=1
      do k=1,nl
       do i=1,nx
          workst(ii)=x(i,3,k)
          ii=ii+1
       enddo
      enddo
      call mpi_isend(workst,nx*nl,mpi_double_precision,itop,10,mpicom,isend1,ierr)
      call mpi_irecv(workrb,nx*nl,mpi_double_precision,ibot,10,mpicom,irecv1,ierr)
!-----
      do 530 k = 1,nl
      DO 530 J = JBeg,JEnd
      DO 530 I = 3 ,IE
      VU(I,J,k)  = VU(I,J,k) + VC31( HU(I,J),FU(I,J,k)
     &         ,    (X(I-2,J,k)+X(I+1,J,k)-X(I-1,J,k)-X(I,J,k))
     &         / (EP+X(I-2,J,k)+X(I+1,J,k)+X(I-1,J,k)+X(I,J,k)) )
530   CONTINUE

      do 535 k=1,nl
      DO 535 J = JBeg,JEnd
      VU(2,J,k)  = VU(2,J,k) + VC31( HU(2,J),FU(2,J,k)
     &         ,    (X(IM,J,k)+X(3,J,k)-X(1,J,k)-X(2,J,k))
     &         / (EP+X(IM,J,k)+X(3,J,k)+X(1,J,k)+X(2,J,k)) )
535   CONTINUE

      do 540 k = 1,nl
      DO 540 J = JBeg,JEnd
      DO 540 I = IB,IE
      VU(I,J,k)  = VU(I,J,k) + VC32( HU(I,J),FU(I,J,k)
     &         ,    (FV(I-1,J-1,k)+FV(I-1,J,k)+FV(I,J-1,k)+FV(I,J,k))
     &         ,    (X(I,J+1,k)-X(I,J-1,k)-X(I-1,J+1,k)+X(I-1,J-1,k))
     &         / (EP+X(I,J+1,k)+X(I,J-1,k)+X(I-1,J+1,k)+X(I-1,J-1,k)) )
540   CONTINUE

C               LATITUDINAL  DIRECTION

      call mpi_wait(isend1,istatus,ierr)
      call mpi_wait(irecv1,istatus,ierr)

      ii=1
      do k=1,nl
       do i=1,nx
          x(i,ny+1,k)=workrb(ii)
          ii=ii+1
       enddo
      enddo

      endj=jend
      if (myrank.eq.0) endj=jend-1
      if ( (myrank.eq.1).and.(nprocs.ge.32) ) endj=jend-1

      do 550 k = 1,nl
      DO 550 J = JBeg,endj
      DO 550 I = IB,IE
      VV(I,J,k)  = VV(I,J,k) + VC31( HV(I,J),FV(I,J,k)
     &         ,    (X(I,J-1,k)+X(I,J+2,k)-X(I,J,k)-X(I,J+1,k))
     &         / (EP+X(I,J-1,k)+X(I,J+2,k)+X(I,J,k)+X(I,J+1,k)) )
550   CONTINUE

!--------------------------------------
C     B.C. BY THE SPHERICAL CYCLICITY

      if (myrank.eq.nprocs-1) then
        do k=1,nl
         do i=ib,ie
            C(I,1,k)  = X(IP(I),jpole+1,k)
           VV(I,jpole,k)  = VV(I,jpole,k) + VC31( HV(I,jpole),FV(I,jpole,k)
     &             ,    (C(I,1,k)+X(I,jpole+2,k)-X(I,jpole,k)-X(I,jpole+1,k))
     &             / (EP+C(I,1,k)+X(I,jpole+2,k)+X(I,jpole,k)+X(I,jpole+1,k)) )
         enddo
        enddo
      endif
!--
      if (nprocs.le.30) then
         if (myrank.eq.0) then
            do k=1,nl
            do i=ib,ie
               C(I,2,k)   = X(IP(I),jpole-1,k)
              VV(I,jpole-1,k)  = VV(I,jpole-1,k) + VC31( HV(I,jpole-1),FV(I,jpole-1,k)
     &           ,    (X(I,jpole-2,k)+C(I,2,k)-X(I,jpole-1,k)-X(I,jpole,k))
     &           / (EP+X(I,jpole-2,k)+C(I,2,k)+X(I,jpole-1,k)+X(I,jpole,k)) )
            enddo
            enddo
         endif
      else
         if (myrank.eq.1) then
            do k=1,nl
            do i=ib,ie
               C(I,2,k)   = X(IP(I),jend,k)
               VV(I,JEnd,k)  = VV(I,JEnd,k) + VC31( HV(I,JEnd),FV(I,JEnd,k)
     &         ,    (X(I,JEnd-1,k)+C(I,2,k)-X(I,JEnd,k)-X(I,NY,k))
     &         / (EP+X(I,JEnd-1,k)+C(I,2,k)+X(I,JEnd,k)+X(I,NY,k)) )
            enddo
            enddo
         endif
      endif
!--------------------


      do 565 k = 1,nl
      DO 565 J = 2 ,JEnd
      DO 565 I = IB,IE
      VV(I,J,k)  = VV(I,J,k) + VC32( HV(I,J),FV(I,J,k)
     &         ,    (FU(I,J,k)+FU(I+1,J,k)+FU(I+1,J+1,k)+FU(I,J+1,k))
     &         ,    (X(I+1,J+1,k)-X(I-1,J+1,k)-X(I+1,J,k)+X(I-1,J,k))
     &         / (EP+X(I+1,J+1,k)+X(I-1,J+1,k)+X(I+1,J,k)+X(I-1,J,k)) )
565   CONTINUE

      ENDIF
C
      DO 570 k = 1,nl
      DO 570 J = JBeg,JEnd
      DO 570 I = IB,IE
      VU(I,J,k)  = SIGN(ONE,VU(I,J,k))*MIN(ABS(UU(I,J,k)),ABS(VU(I,J,k)))
570   CONTINUE

      DO 580 k = 1 ,nl
      DO 580 J = 2 ,JEnd
      DO 580 I = IB,IE
      VV(I,J,k)  = SIGN(ONE,VV(I,J,k))*MIN(ABS(UV(I,J,k)),ABS(VV(I,J,k)))
580   CONTINUE

C     B.C. BY THE SPHERICAL CYCLICITY

      DO 590 k = 1,nl
      DO 590 J = JBeg,JEnd
      VU(1 ,J,k) = VU(IE,J,k)
      VU(NX,J,k) = VU(IB,J,k)
590   CONTINUE

      DO 595 k = 1 ,nl
      DO 595 J = 2 ,JEnd
      VV(1 ,J,k) = VV(IE,J,k)
      VV(NX,J,k) = VV(IB,J,k)
595   CONTINUE
C
C     PERFORM THE NON-OSSCILATORY OPTION
      IF( NONOS.EQ.1 ) THEN

      DO 600 k = 1,nl
      DO 600 J = JBeg,JEnd
      DO 600 I = IB,IE
      XM(I,J,k)  = MAX( X(I-1,J,k),X(I,J,k),X(I+1,J,k),X(I,J-1,k),X(I,J+1,k)
     &         ,        XM(I,J,k) )
      XN(I,J,k)  = MIN( X(I-1,J,k),X(I,J,k),X(I+1,J,k),X(I,J-1,k),X(I,J+1,k)
     &         ,        XN(I,J,k) )
600   CONTINUE
C
      DO 620 k = 1,nl
      DO 620 J = JBeg,JEnd
      DO 620 I = IB,IE
Cb    FU(I,J,k)  = DONOR( X(I-1,J,k),X(I,J,k),VU(I,J,k) )
      IF(VU(I,J,k).GE.0.0E0) THEN
        FU(I,J,k)= X(I-1,J,k)*VU(I,J,k)
      ELSE
        FU(I,J,k)= X(I,J,k)*VU(I,J,k)
      ENDIF
620   CONTINUE

      DO 625 k = 1,nl
      DO 625 J = JBeg,JEnd
      FU(NX,J,k) = FU(IB,J,k)
625   CONTINUE

      DO 630 k = 1 ,nl
      DO 630 J = 2 ,JEnd
      DO 630 I = IB,IE
      IF(VV(I,J,k).GE.0.0E0) THEN
        FV(I,J,k)= X(I,J,k)*VV(I,J,k)
      ELSE
        FV(I,J,k)= X(I,J+1,k)*VV(I,J,k)
      ENDIF
630   CONTINUE

!--------------
       ii=1
       do k=1,nl
        do i=1,nx
           worksb(ii)=fv(i,ny-1,k)
           ii=ii+1
        enddo
       enddo
        call mpi_isend(worksb,nx*nl,mpi_double_precision,ibot,12,mpicom,isend1,ierr)
        call mpi_irecv(workrt,nx*nl,mpi_double_precision,itop,12,mpicom,irecv1,ierr)
        call mpi_wait(isend1,istatus,ierr)
        call mpi_wait(irecv1,istatus,ierr)
       ii=1
       do k=1,nl
        do i=1,nx
           fv(i,1,k)=workrt(ii)
           ii=ii+1
        enddo
       enddo

      DO 640 k = 1,nl
      DO 640 J = JBeg,JEnd
      DO 640 I = IB,IE
      BU(I,J,k)  = (XM(I,J,k)-X(I,J,k))*H(I,J) /
     &  (PN(FU(I+1,J,k))+PP(FU(I,J,k))+PN(FV(I,J,k))+PP(FV(I,J-1,k))+EP)
      BD(I,J,k)  = (X(I,J,k)-XN(I,J,k))*H(I,J) /
     &  (PP(FU(I+1,J,k))+PN(FU(I,J,k))+PP(FV(I,J,k))+PN(FV(I,J-1,k))+EP)
640   CONTINUE

      DO 645 k = 1,nl
      DO 645 J = JBeg,JEnd
      BU(1,J,k)  = BU(IE,J,k)
      BD(1,J,k)  = BD(IE,J,k)
645   CONTINUE
C     KEEP IN MIND THAT H ARE ZERO AT POLE

      if (inc_pole) then
         do k=1,nl
          do i=1,ie
           BU(I,jpole,k) = ZERO
           BD(I,jpole,k) = ZERO
          enddo
         enddo
      endif
!----
      ii=1
      do k=1,nl
       do i=1,nx
          workst(ii)=bu(i,2,k)
          worksb(ii)=bd(i,2,k)
          ii=ii+1
       enddo
      enddo

        call mpi_isend(workst,nx*nl,mpi_double_precision,itop,13,mpicom,isend1,ierr)
        call mpi_isend(worksb,nx*nl,mpi_double_precision,itop,14,mpicom,isend2,ierr)
        call mpi_irecv(workrb,nx*nl,mpi_double_precision,ibot,13,mpicom,irecv1,ierr)
        call mpi_irecv(workrt,nx*nl,mpi_double_precision,ibot,14,mpicom,irecv2,ierr)
!----
      DO 660 k = 1,nl
      DO 660 J = JBeg,JEnd
      DO 660 I = IB,IE
      VU(I,J,k)  = PP( VU(I,J,k) ) * MIN(ONE,BD(I-1,J,k),BU(I,J,k))
     &         - PN( VU(I,J,k) ) * MIN(ONE,BU(I-1,J,k),BD(I,J,k))
660   CONTINUE
!----
        call mpi_wait( isend1,istatus,ierr )
        call mpi_wait( irecv1,istatus,ierr )
        call mpi_wait( isend2,istatus,ierr )
        call mpi_wait( irecv2,istatus,ierr )

      ii=1
      do k=1,nl
       do i=1,nx
          bu(i,ny,k)=workrb(ii)
          bd(i,ny,k)=workrt(ii)
          ii=ii+1
       enddo
      enddo
!-----
      DO 670 k = 1 ,nl
      DO 670 J = 2 ,JEnd
      DO 670 I = IB,IE
      VV(I,J,k)  = PP( VV(I,J,k) ) * MIN(ONE,BD(I,J,k),BU(I,J+1,k))
     &         - PN( VV(I,J,k) ) * MIN(ONE,BU(I,J,k),BD(I,J+1,k))
670   CONTINUE

C     B.C. BY THE SPHERICAL CYCLICITY

      DO 680 k = 1,nl
      DO 680 J = JBeg,JEnd
      VU(1 ,J,k) = VU(IE,J,k)
      VU(NX,J,k) = VU(IB,J,k)
680   CONTINUE

      do 685 k = 1,nl
      DO 685 J = 2 ,JEnd
      VV(1 ,J,k) = VV(IE,J,k)
      VV(NX,J,k) = VV(IB,J,k)
685   CONTINUE

      ENDIF

700   CONTINUE
C


C     UPDATE THE PREDICTED FIELD
      do 800 k = 1,nl
      DO 800 J = 2 ,NY-1
      DO 800 I = 1 ,NX
      Q(I,J,K) = X(I,J,k)
800   CONTINUE

!- check ---------------------------------------------------------
!
!      write(filename,14) 'mpdata-p-',myrank,'.out'
!14    format(a9,i1,a4)
!
!      open (10,file=trim(filename))
!
!      do j=1,ny
!        write(10,11) j,q(1,j,10),q(2,j,10)
!      enddo
!
!11    format(1x,i5,2e30.20)
!      close (10)
!
!      call mpi_finalize(j)
!      stop'mpdata'
!--------------------------------------------------------------

      RETURN
	END

# 1101 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/MPDATA.F"

