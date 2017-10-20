# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/HDIFUS.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/HDIFUS.F"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/HDIFUS.F" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/HDIFUS.F" 2

!!(2003.11.12-13)
!!(2004.03.30 wh)
!!-------------------



*     SUBROUTINE HDIFUS(U,V,T,QV,QC,QR,QI,QS,QG
      SUBROUTINE HDIFUS(U,V,T,QV
     _                 ,FRDT,FRDS,FRDU,FRDV,FRDP,TB,PLY,DXVPN,DXVPS)
C     *****************
C     *****************
C
C     COMPUTE HORIZONTAL DIFFUSION
C     INCLOUDING: T & U & V & QV & QC & QR & QI & QS & QG
C             NON-LINEAR DIFFUSION OF           Smagorinsky (1963)
C             FORMULATED     AS W. M. Washington & D .L .Wiliamson
C             1977 :  A description of the NCAR global circulation
C             models, Methods in Computational Physics  17  113-73
C             DESCRETED IN C-GRID SYSTEM            BY X._Z. Liang
C             1986 :  The Design of IAP GCM and  the Simulation of
C                     Climate  and   Its Interseasonal Variability
C                     Ph.D Thesis                            250pp
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
# 32 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/HDIFUS.F" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/PARADD" 1
      INTEGER IB,IE,JB,JE,KE,NM
      PARAMETER ( IB=2,IE=NX-1,JB=2,JE=NY-1,KE=NL+2,NM=NL-1 )
# 33 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/HDIFUS.F" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/commpi.h" 1

        
      integer nprocs, myrank, ierr, itop, ibot, jbeg, jend, jpole
      logical inc_pole

      common/commpi/ nprocs, myrank, itop, ibot, jbeg, jend, jpole, inc_pole
# 34 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/HDIFUS.F" 2

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
# 35 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/HDIFUS.F" 2

      real*8 workst1(  nx*nl),workrb1(  nx*nl),worksb1(3*nx*nl),workrt1(3*nx*nl)
!!    real workst2(6*nx*nl),workrb2(6*nx*nl),worksb2(6*nx*nl),workrt2(6*nx*nl) !!( 2004.03.30)
      real*8 workst2(  nx*nl),workrb2(  nx*nl),worksb2(  nx*nl),workrt2(  nx*nl) !!
      real*8 workst3(3*nx*nl),workrb3(3*nx*nl),worksb3(  nx*nl),workrt3(  nx*nl)
      real*8                                   worksb4(  nx*nl),workrt4(  nx*nl)

      integer isend11,irecv11
      integer isend12,irecv12
      integer isend21,irecv21
      integer isend22,irecv22
      integer isend31,irecv31
      integer isend32,irecv32
      integer isend42,irecv42

      integer istatus(mpi_status_size)
!      integer ii

      character*50 filename

C
      REAL*8 FRDT(NY,3),FRDS(NY,3),FRDU(NY,3),FRDV(NY,3),FRDP(NY,3)
C
      REAL*8 U(NX,NY,NL),V(NX,NY,NL),T(NX,NY,NL)

!!    REAL QV(NX,NY,NL),QC(NX,NY,NL),QR(NX,NY,NL)
!!   _    ,QI(NX,NY,NL),QS(NX,NY,NL),QG(NX,NY,NL)
      REAL*8 QV(NX,NY,NL)                              !!(wh 2004.03.30)

      REAL*8 TB(NX,NY,NL),PLY(NX,NY,NZ),DXVPN(NY),DXVPS(NY)
C
      REAL*8         D(NX,NY,nl),DT(NX,NY,nl),DS(NX,NY,nl),DA(NX,NY,nl),DB(NX,NY,nl)
     &       ,      VR(NX,NY,nl),QK(NX,NY,nl),TK(NX,NY,nl),VK(NX,NY,nl),UK(NX,NY,nl)
     &       ,      ROT(NX,NY,nl), RLNT(NX,NY,nl), RDLN(NX,NY,nl), RDLT(NX,NY,nl)
     &       ,      TW (NX,NY,nl)                    !!(wh 2004.03.30)
!!   &       ,      TW (NX,NY,nl),QKC(NX,NY,nl),QKR(NX,NY,nl),QKI(NX,NY,nl)
!!   &       ,      QKS(NX,NY,nl),QKG(NX,NY,nl)
C
      REAL*8  ZERO,HALF,ONE
      DATA       ZERO,HALF,ONE / 0.0E0 , 0.5E0 , 1.0E0  /
      REAL*8  FRDTN,FRDTS,FRDSI,FRDSJ,TI,RIJ,RI,VRI,TDI,FT1,FT2,FT3
      real*8    FS1,FS2,FS3,U0,V0,DTN,DTS,DTJ,DSI,DIJ,R0,R1,D0,D1,RT
      real*8    RN,DIJ1,DIJ2,FU1,FU2,FU3,FV1,FV2,FV3,FA1,FA2,FA3,VR0
      real*8    DS0,DT0,DA0,DB0,RLNT0,ROT0,RTA0,RSB0,VRU,VRV,RTAI,RSBI
!!   &       ,RTAJ,RSBJ,FB1,FB2,FB3,FB4,TD0,Q0,Q0C,Q0R,Q0I,Q0S,Q0G
!!   &       ,UIJK,VIJK,TIJK,QIJK,QIJKC,QIJKR,QIJKI,QIJKS,QIJKG
!!   &       ,VRN,VRS,TDN,TDS,QKN,QKNC,QKNR,QKNI,QKNS,QKNG
!!   &       ,QKSV,QKSC,QKSR,QKSI,QKSS,QKSG,DQN,DQNC,DQNR,DQNI,DQNS,DQNG
!!   &       ,DQS,DQSC,DQSR,DQSI,DQSS,DQSG,FAN,FAS,EAN,EAS,T0N
!!   &       ,T0S,Q0N,Q0NC,Q0NR,Q0NI,Q0NS,Q0NG
!!   &       ,Q0S0,Q0SC,Q0SR,Q0SI,Q0SS,Q0SG,FVI,FVJ,DXVPNJ,DXVPSJ
!!                                                                !!(wh 2004.03.30)
     &       ,RTAJ,RSBJ,FB1,FB2,FB3,FB4,TD0,Q0
     &       ,UIJK,VIJK,TIJK,QIJK
     &       ,VRN,VRS,TDN,TDS,QKN
     &       ,QKSV,DQN
     &       ,DQS,FAN,FAS,EAN,EAS,T0N
     &       ,T0S,Q0N
     &       ,Q0S0,FVI,FVJ,DXVPNJ,DXVPSJ

      INTEGER I,J,K,J1,JJ,I1,II
CWB
C
      if (myrank.eq.nprocs-1) then    ! the north pole
          FRDTN     = FRDT(jpole ,1)
          FRDSI     = FRDS(jpole ,1)
          FRDSJ     = FRDS(jpole ,2)
      endif

      if (myrank.eq.0) then           ! the south pole
          FRDTS     = FRDT(jpole,1)
      endif
!
C
C     CALCULATE DENSITY & SET 2-D FIELDS TO BE DIFFUSED
C
      do 300 k = 1 ,nl
      DO 300 J = 2 ,NY-1
      DO 300 I = 1 ,NX
      UK(I,J,k)  = U(I,J,K)
      VK(I,J,k)  = V(I,J,K)
      QK(I,J,k)  = QV(I,J,K)
*     QKC(I,J)  = QC(I,J,K)
*     QKR(I,J)  = QR(I,J,K)
*     QKI(I,J)  = QI(I,J,K)
*     QKS(I,J)  = QS(I,J,K)
*     QKG(I,J)  = QG(I,J,K)
!!      QKC(I,J,k)  = 0.0
!!      QKR(I,J,k)  = 0.0
!!      QKI(I,J,k)  = 0.0  !!(wh 2004.03.30)
!!      QKS(I,J,k)  = 0.0
!!      QKG(I,J,k)  = 0.0
      TW(I,J,k)  = T(I,J,K)
300   CONTINUE

      ii=1
      do k=1,nl
       do i=1,nx
          workst1(ii)=uk(i,2,   k)
          worksb1(ii)=vk(i,ny-1,k)
          ii=ii+1
       enddo
      enddo

      call mpi_isend( workst1,nx*nl,mpi_double_precision,itop,11,mpicom,isend11,ierr)
      call mpi_irecv( workrb1,nx*nl,mpi_double_precision,ibot,11,mpicom,irecv11,ierr)
      call mpi_isend( worksb1,nx*nl,mpi_double_precision,ibot,12,mpicom,isend12,ierr)
      call mpi_irecv( workrt1,nx*nl,mpi_double_precision,itop,12,mpicom,irecv12,ierr)

      ii=1
!--1
      do k=1,nl
       do i=1,nx
          workst2(ii)=qk(i,2,   k)
          worksb2(ii)=qk(i,ny-1,k)
          ii=ii+1
       enddo
      enddo
!!--2
!!      do k=1,nl
!!       do i=1,nx
!!          workst2(ii)=qkc(i,2,   k)
!!          worksb2(ii)=qkc(i,ny-1,k)
!!          ii=ii+1
!!       enddo
!!      enddo
!!!--3
!!      do k=1,nl
!!       do i=1,nx
!!          workst2(ii)=qkr(i,2,   k)
!!          worksb2(ii)=qkr(i,ny-1,k)
!!          ii=ii+1
!!       enddo
!!      enddo
!!!--4
!!      do k=1,nl
!!       do i=1,nx
!!          workst2(ii)=qki(i,2,   k)
!!          worksb2(ii)=qki(i,ny-1,k)
!!          ii=ii+1
!!       enddo
!!      enddo
!!!!--5
!!      do k=1,nl
!!       do i=1,nx
!!          workst2(ii)=qks(i,2,   k)
!!          worksb2(ii)=qks(i,ny-1,k)
!!          ii=ii+1
!!       enddo
!!      enddo
!!!--6
!!      do k=1,nl
!!       do i=1,nx
!!          workst2(ii)=qkg(i,2,   k)
!!          worksb2(ii)=qkg(i,ny-1,k)
!!          ii=ii+1
!!       enddo
!!      enddo
!!
!!      call mpi_isend( workst2,6*nx*nl,mpi_double_precision,itop,21,mpicom,isend21,ierr)
!!      call mpi_irecv( workrb2,6*nx*nl,mpi_double_precision,ibot,21,mpicom,irecv21,ierr)
!!      call mpi_isend( worksb2,6*nx*nl,mpi_double_precision,ibot,22,mpicom,isend22,ierr)
!!      call mpi_irecv( workrt2,6*nx*nl,mpi_double_precision,itop,22,mpicom,irecv22,ierr)

      call mpi_isend( workst2,nx*nl,mpi_double_precision,itop,21,mpicom,isend21,ierr)
      call mpi_irecv( workrb2,nx*nl,mpi_double_precision,ibot,21,mpicom,irecv21,ierr)
      call mpi_isend( worksb2,nx*nl,mpi_double_precision,ibot,22,mpicom,isend22,ierr)
      call mpi_irecv( workrt2,nx*nl,mpi_double_precision,itop,22,mpicom,irecv22,ierr)
!-----------------------------------

      do k = 1,nl

         DO 325 J  = JBeg,JEnd
         DO 325 I  = IB,IE
            TI        = TW(I,J,k)
            RIJ       = PLY(I,J,K) / (TI*RD)
            ROT(I,J,k)  = RIJ
            VR (I,J,k)  = ONE / RIJ
            TK (I,J,k)  = TI  - TB(I,J,K)
325      CONTINUE

         if (inc_pole) then
            TI        = TW (IB,Jpole,k)
            RI        = PLY(IB,Jpole,K) / (TI*RD)
            VRI       = ONE / RI
            TDI       = TI  - TB(IB,Jpole,K)
            DO 350 I  = IB,IE
            ROT(I,Jpole,k)  = RI
            VR (I,Jpole,k)  = VRI
            TK (I,Jpole,k)  = TDI
350      CONTINUE
         endif

         DO 375 J  = 2 ,NY-1
            ROT(1 ,J,k) = ROT(IE,J,k)
            ROT(NX,J,k) = ROT(IB,J,k)
            VR (1 ,J,k) = VR (IE,J,k)
            VR (NX,J,k) = VR (IB,J,k)
            TK (1 ,J,k) = TK (IE,J,k)
            TK (NX,J,k) = TK (IB,J,k)
375      CONTINUE
      enddo
!---
      ii=1
      do k=1,nl
       do i=1,nx
          workst3(ii)=tk(i,2,   k)
          worksb3(ii)=tk(i,ny-1,k)
          ii=ii+1
       enddo
      enddo
      do k=1,nl
       do i=1,nx
          workst3(ii)=vr(i,2,   k)
          ii=ii+1
       enddo
      enddo
      do k=1,nl
       do i=1,nx
          workst3(ii)=rot(i,2,   k)
          ii=ii+1
       enddo
      enddo

      call mpi_isend( workst3,3*nx*nl,mpi_double_precision,itop,31,mpicom,isend31,ierr)
      call mpi_irecv( workrb3,3*nx*nl,mpi_double_precision,ibot,31,mpicom,irecv31,ierr)
      call mpi_isend( worksb3,  nx*nl,mpi_double_precision,ibot,32,mpicom,isend32,ierr)
      call mpi_irecv( workrt3,  nx*nl,mpi_double_precision,itop,32,mpicom,irecv32,ierr)
!----------------------------------
C
C     CALCULATE DEFORMATION FIELDS
C

      call mpi_wait(isend11,istatus,ierr)
      call mpi_wait(irecv11,istatus,ierr)
      call mpi_wait(isend12,istatus,ierr)
      call mpi_wait(irecv12,istatus,ierr)
      ii=1
      do k=1,nl
       do i=1,nx
          uk(i,ny,k)=workrb1(ii)
          vk(i,1, k)=workrt1(ii)
          ii=ii+1
       enddo
      enddo
!--

      do k=1,nl

      DO 400 J  = JBeg,JEnd
      J1        = J - 1
      JJ        = J + 1
      FT1       = FRDT(J,1)
      FT2       = FRDT(J,2)
      FT3       = FRDT(J,3)
      FS1       = FRDS(J,1)
      FS2       = FRDS(J,2)
      FS3       = FRDS(J,3)
      DO 400 I  = IB,IE
      U0        = UK(I,J,k)
      V0        = VK(I,J,k)
      DT(I,J,k)   = FT1*(UK(I+1,J,k)-U0) - (FT2*V0 - FT3*VK(I,J1,k ))
      DS(I,J,k)   = FS1*(V0-VK(I-1,J,k)) + (FS2*UK(I,JJ,k) - FS3*U0)
400   CONTINUE

      if (myrank.eq.nprocs-1) then
          DTN       = ZERO
          do i=ib,ie
            DTN       = DTN + VK(I,jpole,k)
          enddo
            DTN       = DTN * FRDTN
          do i=ib,ie
            DT(I,jpole,k) = DTN
            DS(I,jpole,k) = FRDSI*(VK(I,jpole,k) -VK(I-1,jpole,k))+FRDSJ*UK(I,jpole+1,k)
          enddo
      endif

      if (myrank.eq.0) then
         DTS       = ZERO
         do i=ib,ie
            DTS       = DTS + VK(I,jpole-1,k)
         enddo
            DTS       = DTS * FRDTS
         DO  I  = IB,IE
            DT(I,jpole,k)  = DTS
            DS(I,jpole,k)  = ZERO
         enddo
      endif

      DO 475 J  = 2 ,NY-1
      DT(1 ,J,k)  = DT(IE,J,k)
      DT(NX,J,k)  = DT(IB,J,k)
      DS(1 ,J,k)  = DS(IE,J,k)
      DS(NX,J,k)  = DS(IB,J,k)
475   CONTINUE

      enddo
!------------------ send DS ----
      ii=1
      do k=1,nl
       do i=1,nx
          worksb4(ii)=ds(i,ny-1,k)
          ii=ii+1
       enddo
      enddo

      call mpi_isend( worksb4,nx*nl,mpi_double_precision,ibot,42,mpicom,isend42,ierr)
      call mpi_irecv( workrt4,nx*nl,mpi_double_precision,itop,42,mpicom,irecv42,ierr)

!----------------------send & recv DT-------
      ii=1
      do k=1,nl
       do i=1,nx
          workst1(ii)=dt(i,2,k)
          ii=ii+1
       enddo
      enddo

      call mpi_isend( workst1,nx*nl,mpi_double_precision,itop,11,mpicom,isend11,ierr)
      call mpi_irecv( workrb1,nx*nl,mpi_double_precision,ibot,11,mpicom,irecv11,ierr)
      call mpi_wait(isend11,istatus,ierr)
      call mpi_wait(irecv11,istatus,ierr)

      ii=1
      do k=1,nl
       do i=1,nx
          dt(i,ny,k)=workrb1(ii)
          ii=ii+1
       enddo
      enddo
!-----

      do k=1,nl

      DO 500 J  = 2 ,JEnd
      JJ        = J + 1
      DO 525 I  = IB,IE
      DTJ       = DT(I,JJ,k) + DT(I,J,k)
      DSI       = DS(I,J ,k) + DS(I-1,J,k)
      DIJ       = DTJ*DTJ  + DSI*DSI
      D(I,J,k)    = HALF * SQRT( DIJ )
525   CONTINUE
      D(1 ,J,k)   = D(IE,J,k)
      D(NX,J,k)   = D(IB,J,k)
500   CONTINUE

      if (myrank.eq.0) then
          DO I = 1 ,NX
            D(I,jpole,k) = ZERO
          ENDDO
      endif

      enddo

!------------------ send & recv D ----
      ii=1
      do k=1,nl
       do i=1,nx
          worksb1(ii)=d(i,ny-1,k)
          ii=ii+1
       enddo
      enddo

      call mpi_isend( worksb1,nx*nl,mpi_double_precision,ibot,12,mpicom,isend12,ierr)
      call mpi_irecv( workrt1,nx*nl,mpi_double_precision,itop,12,mpicom,irecv12,ierr)
      call mpi_wait( isend12,istatus,ierr)
      call mpi_wait( irecv12,istatus,ierr)

      ii=1
      do k=1,nl
       do i=1,nx
          d(i,1,k)=workrt1(ii)
          ii=ii+1
       enddo
      enddo
!------------------- recv TK,VR,ROT-------

      call mpi_wait(isend31,istatus,ierr)
      call mpi_wait(irecv31,istatus,ierr)
      call mpi_wait(isend32,istatus,ierr)
      call mpi_wait(irecv32,istatus,ierr)

      ii=1
      do k=1,nl
       do i=1,nx
          tk(i,ny,k)=workrb3(ii)
          tk(i,1, k)=workrt3(ii)
          ii=ii+1
       enddo
      enddo
      do k=1,nl
       do i=1,nx
          vr(i,ny,k)=workrb3(ii)
          ii=ii+1
       enddo
      enddo
      do k=1,nl
       do i=1,nx
          rot(i,ny,k)=workrb3(ii)
          ii=ii+1
       enddo
      enddo
!-----------------------------------------------------------

      do k=1,nl

      DO 550 J  = JBeg,JEnd
      JJ        = J + 1
      J1        = J - 1
      DO 550 I  = IB,IE
      I1        = I - 1
      R0        = ROT(I,J,k)
      R1        = ROT(I,JJ,k)
      D0        = D(I,J,k)
      D1        = D(I,J1,k)
      RT        = R0 + R1
      RN        = R0 + ROT(I1,J,k)
      RLNT(I,J,k) = RN + R1 + ROT(I1,JJ,k)
      DIJ1      = HALF * (D0 + D(I1,J,k))
      DIJ       = HALF * (D0 + D1)
      DIJ2      = HALF * (DIJ1 + HALF*(D1 + D(I1,J1,k)) )
      RDLN(I,J,k) = RN   *  DIJ2
      RDLT(I,J,k) = RT   *  D0
      DA  (I,J,k) = DIJ
      DB  (I,J,k) = DIJ1
550   CONTINUE

      if (inc_pole) then
        DO I = 1 ,NX
          RDLN(I,Jpole,k) = ZERO
          DA(I,Jpole,k)   = ZERO
        ENDDO
      endif

      if (myrank.eq.nprocs-1) then
         R0        = ROT (IB,jpole,k)
         DO 560 I  = IB,IE
            D0        = D(I,jpole,k)
            DB  (I,jpole,k) = (D0 + D(I-1,jpole ,k)) * HALF
            RDLT(I,jpole,k) = (R0 + ROT(I,Jpole+1,k)) * D0
            RLNT(I,jpole,k) =  R0 + R0 + ROT(I-1,Jpole+1,k) + ROT(I,Jpole+1,k)
560      CONTINUE
      endif

      if (myrank.eq.0) then
         DO I = 1 ,NX
           RDLT(I,jpole,k) = ZERO
           RLNT(I,jpole,k) = ZERO
           DB(I,jpole,k)   = ZERO
         ENDDO
      endif

      DO 580 J  = 2 ,JEnd
      RLNT(1 ,J,k)= RLNT(IE,J,k)
      RLNT(NX,J,k)= RLNT(IB,J,k)
      RDLN(1 ,J,k)= RDLN(IE,J,k)
      RDLN(NX,J,k)= RDLN(IB,J,k)
      RDLT(1 ,J,k)= RDLT(IE,J,k)
      RDLT(NX,J,k)= RDLT(IB,J,k)
      DA  (1 ,J,k)= DA  (IE,J,k)
      DA  (NX,J,k)= DA  (IB,J,k)
      DB  (1 ,J,k)= DB  (IE,J,k)
      DB  (NX,J,k)= DB  (IB,J,k)
580   CONTINUE

      enddo
!---------------------------------------------------------------------

C     UPDATE T & U & V & QV,QC,QI,QR,QS,QG DUE TO THE HORIZONTAL DIFFUSION
C

      ii=1
      do k=1,nl
       do i=1,nx
          workst1(ii)=da(i,2,k)
          worksb1(ii)=db(i,ny-1,k)
          ii=ii+1
       enddo
      enddo
      do k=1,nl
       do i=1,nx
          worksb1(ii)=rdlt(i,ny-1,k)
          ii=ii+1
       enddo
      enddo
      do k=1,nl
       do i=1,nx
          worksb1(ii)=rlnt(i,ny-1,k)
          ii=ii+1
       enddo
      enddo

      call mpi_isend( workst1,  nx*nl,mpi_double_precision,itop,11,mpicom,isend11,ierr)
      call mpi_irecv( workrb1,  nx*nl,mpi_double_precision,ibot,11,mpicom,irecv11,ierr)
      call mpi_isend( worksb1,3*nx*nl,mpi_double_precision,ibot,12,mpicom,isend12,ierr)
      call mpi_irecv( workrt1,3*nx*nl,mpi_double_precision,itop,12,mpicom,irecv12,ierr)
      call mpi_wait(isend11,istatus,ierr)
      call mpi_wait(irecv11,istatus,ierr)
      call mpi_wait(isend12,istatus,ierr)
      call mpi_wait(irecv12,istatus,ierr)

      ii=1
      do k=1,nl
       do i=1,nx
          da(i,ny,k)=workrb1(ii)
          db(i,1, k)=workrt1(ii)
          ii=ii+1
       enddo
      enddo
      do k=1,nl
       do i=1,nx
          rdlt(i,1,k)=workrt1(ii)
          ii=ii+1
       enddo
      enddo
      do k=1,nl
       do i=1,nx
          rlnt(i,1,k)=workrt1(ii)
          ii=ii+1
       enddo
      enddo
!-----
      call mpi_wait(isend42,istatus,ierr)
      call mpi_wait(irecv42,istatus,ierr)
      ii=1
      do k=1,nl
       do i=1,nx
          ds(i,1,k)=workrt4(ii)
          ii=ii+1
       enddo
      enddo

!-------------
      call mpi_wait(isend21,istatus,ierr)
      call mpi_wait(irecv21,istatus,ierr)
      call mpi_wait(isend22,istatus,ierr)
      call mpi_wait(irecv22,istatus,ierr)

      ii=1
!--1
      do k=1,nl
       do i=1,nx
          qk(i,ny,k)=workrb2(ii)
          qk(i,1, k)=workrt2(ii)
          ii=ii+1
       enddo
      enddo
!!--2
!!      do k=1,nl
!!       do i=1,nx
!!          qkc(i,ny,k)=workrb2(ii)
!!          qkc(i,1, k)=workrt2(ii)
!!          ii=ii+1
!!       enddo
!!      enddo
!!--3
!!      do k=1,nl
!!!       do i=1,nx
!!          qkr(i,ny,k)=workrb2(ii)
!!          qkr(i,1, k)=workrt2(ii)
!!          ii=ii+1
!!       enddo
!!      enddo
!!!--4
!!      do k=1,nl
!!       do i=1,nx
!!          qki(i,ny,k)=workrb2(ii)
!!          qki(i,1, k)=workrt2(ii)
!!          ii=ii+1
!!       enddo
!!      enddo
!!!--5
!!      do k=1,nl
!!       do i=1,nx
!!          qks(i,ny,k)=workrb2(ii)
!!          qks(i,1, k)=workrt2(ii)
!!          ii=ii+1
!!       enddo
!!      enddo
!!!--6
!!      do k=1,nl
!!       do i=1,nx
!!          qkg(i,ny,k)=workrb2(ii)
!!          qkg(i,1, k)=workrt2(ii)
!!          ii=ii+1
!!       enddo
!!      enddo                   !!(wh 2004.03.30)
!!!-------------------------------------------------
      do k=1,nl

      DO 600 J  = JBeg,JEnd
      FU1       = FRDU(J,1)
      FU2       = FRDU(J,2)
      FU3       = FRDU(J,3)
      FV1       = FRDV(J,1)
      FV2       = FRDV(J,2)
      FV3       = FRDV(J,3)
      FA1       = FRDP(J,1)
      FA2       = FRDP(J,2)
      FA3       = FRDP(J,3)
      JJ        = J + 1
      J1        = J - 1
      DO 600 I  = IB,IE
      II        = I + 1
      I1        = I - 1
      VR0       = VR  (I,J,k)
      DS0       = DS  (I,J,k)
      DT0       = DT  (I,J,k)
      DA0       = DA  (I,J,k)
      DB0       = DB  (I,J,k)
      RLNT0     = RLNT(I,J,k)
      ROT0      = ROT (I,J,k)
      RTA0      = DT0 * DA0 * ROT0
      RSB0      = DS0 * DB0 * RLNT0
      VRU       = VR0 + VR(I1,J,k)
      VRV       = VR0 + VR(I ,JJ,k)
      RTAI      = ROT (I1,J,k)*DT(I1,J,k)*DA(I1,J,k)
      RSBI      = RLNT(I,J1,k)*DS(I,J1,k)*DB(I,J1,k)
      RTAJ      = ROT (I,JJ,k)*DT(I,JJ,k)*DA(I,JJ,k)
      RSBJ      = RLNT(II,J,k)*DS(II,J,k)*DB(II,J,k)
      FB1       = FA1 * RDLN(II,J,k)
      FB2       = FA1 * RDLN(I ,J,k)
      FB3       = FA2 * RDLT(I ,J,k)
      FB4       = FA3 * RDLT(I,J1,k)
      TD0       = TK(I,J,k)
      Q0        = QK(I,J,k)
!!      Q0C       = QKC(I,J,k)
!!      Q0R       = QKR(I,J,k)
!!      Q0I       = QKI(I,J,k)  !!(wh 2004.03.30)
!!      Q0S       = QKS(I,J,k)
!!      Q0G       = QKG(I,J,k)

      UIJK      = VRU * ( FU1*(RTA0-RTAI) + (FU2*RSB0 - FU3*RSBI) )
      VIJK      = VRV * ( FV1*(RSBJ-RSB0) - (FV2*RTAJ - FV3*RTA0) )
      TIJK      = VR0 * ( FB1*(TK(II,J,k)-TD0) - FB2*(TD0-TK(I1,J,k))
     &          +         FB3*(TK(I,JJ,k)-TD0) - FB4*(TD0-TK(I,J1,k)) )
      QIJK      = VR0 * ( FB1*(QK(II,J,k)-Q0 ) - FB2*(Q0 -QK(I1,J,k))
     &          +         FB3*(QK(I,JJ,k)-Q0 ) - FB4*(Q0 -QK(I,J1,k)) )
!!      QIJKC     = VR0 * ( FB1*(QKC(II,J,k)-Q0C) - FB2*(Q0C-QKC(I1,J,k))      !!
!!     &          +         FB3*(QKC(I,JJ,k)-Q0C) - FB4*(Q0C-QKC(I,J1,k)) )    !!
!!      QIJKR     = VR0 * ( FB1*(QKR(II,J,k)-Q0R) - FB2*(Q0R-QKR(I1,J,k))      !!
!!     &          +         FB3*(QKR(I,JJ,k)-Q0R) - FB4*(Q0R-QKR(I,J1,k)) )    !!
!!      QIJKI     = VR0 * ( FB1*(QKI(II,J,k)-Q0I) - FB2*(Q0I-QKI(I1,J,k))      !!(wh 2004.03.30)
!!     &          +         FB3*(QKI(I,JJ,k)-Q0I) - FB4*(Q0I-QKI(I,J1,k)) )    !!
!!      QIJKS     = VR0 * ( FB1*(QKS(II,J,k)-Q0S) - FB2*(Q0S-QKS(I1,J,k))      !!
!!     &          +         FB3*(QKS(I,JJ,k)-Q0S) - FB4*(Q0S-QKS(I,J1,k)) )    !!
!!      QIJKG     = VR0 * ( FB1*(QKG(II,J,k)-Q0G) - FB2*(Q0G-QKG(I1,J,k))      !!
!!     &          +         FB3*(QKG(I,JJ,k)-Q0G) - FB4*(Q0G-QKG(I,J1,k)) )    !!
C
      U(I,J,K)  = UK(I,J,k) + UIJK
      V(I,J,K)  = VK(I,J,k) + VIJK
      T(I,J,K)  = TW(I,J,k) + TIJK
      QV(I,J,K)  = QK (I,J,k) + QIJK
!!      QC(I,J,K)  = QKC(I,J,k) + QIJKC !!
!!      QR(I,J,K)  = QKR(I,J,k) + QIJKR !!
!!      QI(I,J,K)  = QKI(I,J,k) + QIJKI !! (wh 2004.03.30)
!!      QS(I,J,K)  = QKS(I,J,k) + QIJKS !!
!!      QG(I,J,K)  = QKG(I,J,k) + QIJKG !!
600   CONTINUE

      if (myrank.eq.nprocs-1) then
         VRN       = VR(IB,jpole,k)
         TDN       = TK(IB,jpole,k)
         QKN       = QK(IB,jpole,k)
!!         QKNC      = QKC(IB,jpole,k)
!!         QKNR      = QKR(IB,jpole,k)
!!         QKNI      = QKI(IB,jpole,k)  !!(wh 2004.03.30)
!!         QKNS      = QKS(IB,jpole,k)
!!         QKNG      = QKG(IB,jpole,k)
         DQN       = ZERO
!!         DQNC      = ZERO
!!         DQNR      = ZERO
!!         DQNI      = ZERO             !!(wh 2004.03.30)
!!         DQNS      = ZERO
!!         DQNG      = ZERO
         DTN       = ZERO
         FAN       = FRDP(jpole ,1) * VRN
         DO  I  = IB,IE
            EAN       = RDLT(I,jpole,k)
            DTN       = DTN + EAN*(TK(I,Jpole+1,k) - TDN)
            DQN       = DQN + EAN*(QK(I,Jpole+1,k) - QKN)
!!            DQNC      = DQNC+ EAN*(QKC(I,Jpole+1,k) - QKNC)
!!            DQNR      = DQNR+ EAN*(QKR(I,Jpole+1,k) - QKNR)
!!            DQNI      = DQNI+ EAN*(QKI(I,Jpole+1,k) - QKNI)  !(wh 2004.03.30)
!!            DQNS      = DQNS+ EAN*(QKS(I,Jpole+1,k) - QKNS)
!!            DQNG      = DQNG+ EAN*(QKG(I,Jpole+1,k) - QKNG)
         enddo
      endif

      if (myrank.eq.0) then
         VRS       = VR(IB,jpole,k)
         TDS       = TK(IB,jpole,k)
         QKSV      = QK(IB,jpole,k)
!!         QKSC      = QKC(IB,jpole,k)
!!         QKSR      = QKR(IB,jpole,k)
!!         QKSI      = QKI(IB,jpole,k)  !!(wh 2004.03.30)
!!         QKSS      = QKS(IB,jpole,k)
!!         QKSG      = QKG(IB,jpole,k)
         DQS       = ZERO
!!         DQSC      = ZERO
!!         DQSR      = ZERO
!!         DQSI      = ZERO
!!         DQSS      = ZERO
!!         DQSG      = ZERO
         DTS       = ZERO
         FAS       = FRDP(jpole,1) * VRS
         DO  I  = IB,IE
            EAS       = RDLT(I,Jpole-1,k)
            DTS       = DTS + EAS*(TDS - TK(I,Jpole-1,k))
            DQS       = DQS + EAS*(QKSV- QK(I,Jpole-1,k))
!!            DQSC      = DQSC+ EAS*(QKSC - QKC(I,Jpole-1,k))
!!            DQSR      = DQSR+ EAS*(QKSR - QKR(I,Jpole-1,k))
!!            DQSI      = DQSI+ EAS*(QKSI - QKI(I,Jpole-1,k))
!!            DQSS      = DQSS+ EAS*(QKSS - QKS(I,Jpole-1,k))
!!            DQSG      = DQSG+ EAS*(QKSG - QKG(I,Jpole-1,k))
         enddo
      endif

      if (myrank.eq.nprocs-1) then
         T0N       = TW(IB,jpole,k) + DTN*FAN
         Q0N       = QK(IB,jpole,k) + DQN*FAN
!!         Q0NC      = QKC(IB,jpole,k) + DQNC*FAN
!!         Q0NR      = QKR(IB,jpole,k) + DQNR*FAN
!!         Q0NI      = QKI(IB,jpole,k) + DQNI*FAN
!!         Q0NS      = QKS(IB,jpole,k) + DQNS*FAN
!!         Q0NG      = QKG(IB,jpole,k) + DQNG*FAN
         FVI       = FRDV(jpole,1)
         FVJ       = FRDV(jpole,2)
         DO  I  = IB,IE
            II        = I + 1
            T (I,jpole,K) = T0N
            QV(I,jpole,K) = Q0N
!!            QC(I,jpole,K) = Q0NC
!!            QR(I,jpole,K) = Q0NR
!!            QI(I,jpole,K) = Q0NI
!!            QS(I,jpole,K) = Q0NS
!!            QG(I,jpole,K) = Q0NG
            U (I,jpole,K) = ZERO
            VRV       = VRN + VR(I,Jpole+1,k)
            RSBI      = RLNT(II,jpole,k)*DS(II,jpole,k)*DB(II,jpole,k)
            RSB0      = RLNT(I ,jpole,k)*DS(I ,jpole,k)*DB(I ,jpole,k)
            RTA0      = ROT (I,jpole+1,k)*DT(I,jpole+1,k)*DA(I,jpole+1,k)
            VIJK      = VRV * ( FVI*(RSBI - RSB0) - FVJ*RTA0 )
            V(I,jpole,K) = VK(I,jpole,k) + VIJK
         enddo
      endif

      if (myrank.eq.0) then
         T0S       = TW(IB,jpole,k) + DTS*FAS
         Q0S0      = QK(IB,jpole,k) + DQS*FAS
!!         Q0SC      = QKC(IB,jpole,k) + DQSC*FAS
!!         Q0SR      = QKR(IB,jpole,k) + DQSR*FAS
!!         Q0SI      = QKI(IB,jpole,k) + DQSI*FAS
!!         Q0SS      = QKS(IB,jpole,k) + DQSS*FAS
!!         Q0SG      = QKG(IB,jpole,k) + DQSG*FAS
         DO  I  = IB,IE
            II        = I + 1
            T (I,jpole,K) = T0S
            QV(I,jpole,K) = Q0S0
!!            QC(I,jpole,K) = Q0SC
!!            QR(I,jpole,K) = Q0SR
!!            QI(I,jpole,K) = Q0SI
!!            QS(I,jpole,K) = Q0SS
!!            QG(I,jpole,K) = Q0SG
            U (I,jpole,K) = ZERO
            V (I,jpole,K) = ZERO
         enddo
      endif

      enddo
!------
C
C     ENERGY CONSERVATION DUE TO FRICTIONS
C
      do k=1,nl
       DO 710 J  = JBeg,JEnd
         DO 700 I  = IB,IE
           DA(I,J,k)   = UK(I,J,k) * (U(I,J,K) - UK(I,J,k))
700      CONTINUE
CWB
           DA(1 ,J,k)  = DA(IE,J,k)
           DA(NX,J,k)  = DA(IB,J,k)
CWB
710    CONTINUE
      enddo
!----
      do 720 k  = 1,nl
      DO 720 J  = 2 ,JEnd
      DO 720 I  = IB,IE
      DB(I,J,k)   = VK(I,J,k) * (V(I,J,K) - VK(I,J,k))
720   CONTINUE

!------------------ send & recv DB ----
      ii=1
      do k=1,nl
       do i=1,nx
          worksb1(ii)=db(i,ny-1,k)
          ii=ii+1
       enddo
      enddo

      call mpi_isend( worksb1,nx*nl,mpi_double_precision,ibot,12,mpicom,isend12,ierr)
      call mpi_irecv( workrt1,nx*nl,mpi_double_precision,itop,12,mpicom,irecv12,ierr)
      call mpi_wait( isend12,istatus,ierr)
      call mpi_wait( irecv12,istatus,ierr)

      ii=1
      do k=1,nl
       do i=1,nx
          db(i,1,k)=workrt1(ii)
          ii=ii+1
       enddo
      enddo
!----
      do k=1,nl
      DO 750 J  = JBeg,JEnd
      JJ        = J - 1
      DXVPNJ    = DXVPN(J)
      DXVPSJ    = DXVPS(J)
      DO 750 I  = IB,IE
      D (I,J,k)   = HALF * (DA(I,J,k) + DA(I+1,J,k))
     &          + DXVPNJ*DB(I,JJ,k) + DXVPSJ*DB(I,J,k)
      TK(I,J,k)   = T(I,J,K)
750   CONTINUE
      DO 800 J  = JBeg,JEnd
      DO 800 I  = IB,IE
      T(I,J,K)  = TK(I,J,k) - D(I,J,k)/CPD
800   CONTINUE
      enddo
C
C     SPHERICAL BOUNDARY CONDITIONS
C
      DO 900 K  = 1 ,NL
      DO 900 J  = 2 ,NY-1
      U(1 ,J,K) = U(IE,J,K)
      U(NX,J,K) = U(IB,J,K)
      V(1 ,J,K) = V(IE,J,K)
      V(NX,J,K) = V(IB,J,K)
      T(1 ,J,K) = T(IE,J,K)
      T(NX,J,K) = T(IB,J,K)
      QV(1 ,J,K) = QV(IE,J,K)
      QV(NX,J,K) = QV(IB,J,K)
!!      QC(1 ,J,K) = QC(IE,J,K)
!!      QC(NX,J,K) = QC(IB,J,K)
!!      QR(1 ,J,K) = QR(IE,J,K)
!!      QR(NX,J,K) = QR(IB,J,K)
!!      QI(1 ,J,K) = QI(IE,J,K)
!!      QI(NX,J,K) = QI(IB,J,K)  !!(wh 2004.03.30)
!!      QS(1 ,J,K) = QS(IE,J,K)
!!      QS(NX,J,K) = QS(IB,J,K)
!!      QG(1 ,J,K) = QG(IE,J,K)
!!      QG(NX,J,K) = QG(IB,J,K)
900   CONTINUE
!
!- check ---------------------------------------------------------
!
!      write(filename,14) 'hdifus-p-',myrank,'.out'
!14    format(a9,i1,a4)
!
!      open (10,file=trim(filename))
!
!      write(10,*)'------------ u----------'
!      do j=1,ny
!        write(10,11) j,u(1,j,10),u(2,j,10)
!      enddo
!
!      write(10,*)'------------ v----------'
!      do j=1,ny
!!        write(10,11) j,v(1,j,10),v(2,j,10)
!      enddo
!
!      write(10,*)'------------ t----------'
!      do j=1,ny
!        write(10,11) j,t(1,j,10),t(2,j,10)
!      enddo
!
!      write(10,*)'------------ qv----------'
!      do j=1,ny
!        write(10,11) j,qv(1,j,10),qv(2,j,10)
!      enddo
!
!11    format(1x,i5,2e30.20)
!      close (10)
!
!!      call mpi_finalize(j)
!!      stop 'hdifus'
!--------------------------------------------------------------


      RETURN
      END

