# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/IAPTRSF.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/IAPTRSF.F"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/IAPTRSF.F" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/IAPTRSF.F" 2

      SUBROUTINE IAPTRSF(US,VS,WS,PS,TS,U,V,P,TT,TB,CB,KP)
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
# 13 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/IAPTRSF.F" 2



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
# 16 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/IAPTRSF.F" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/commpi.h" 1

        
      integer nprocs, myrank, ierr, itop, ibot, jbeg, jend, jpole
      logical inc_pole

      common/commpi/ nprocs, myrank, itop, ibot, jbeg, jend, jpole, inc_pole
# 17 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/IAPTRSF.F" 2

      integer   isend, irecv
      integer   istatus(mpi_status_size)
      character*50 filename


!
!     The file PARA is to define the parameters related to the model resolution:
!     NX is the grid number in longitude
!     NY is the grid number in latitude
!     NL is the number of vertical layers
!
	REAL*8
     _	     US(NX,NY,NL )  !  US = u, zonal wind,    input variable
     _      ,VS(NX,NY,NL )  !  VS = v, meridional wind, input variable
     _      ,WS(NX,NY,NZ )  !  vertical velocity, input variable
     _      ,PS (NX,NY   )  !  Surface pressure, input variable
     _      ,TS (NX,NY,NL)  !  TS = T, temperature, input variable
     _      ,TB (NX,NY,NL)  !  the temperature of the standard atmosphere a
!                           !  at the sigma layers, input constant
     _      ,CB (NX,NY,NL)  !  CB=Sqrt(R*(KK*TBB-dTB/dlnp)), input variable,
!                           !  where, KK=R/Cp, R is a constant
     _      ,U  (NX,NY,NL)  !  U=u*sqrt(Ps), output variable
     _      ,V  (NX,NY,NL)  !	 V=v*sqrt(Ps), output variable
     _      ,P  (NX,NY   )  !  P=sqrt(Ps)  , output variable
     _      ,TT (NX,NY,NL)  !  TT=R*T'*Sqrt(Ps)/CB, output variable
!                           !  where T'=T-TB, T is the temperatur,
     _      ,PX,PY          !  working variables
!
	INTEGER I,J,K,KP
        integer begj,endj


      begj = 2
      endj = ny-1





	IF (KP.EQ.1) THEN
!
        DO J=begj,endj
	  DO I=2,NX-1
	     P(I,J)=SQRT(PS(I,J))
	  END DO
	  PS (1 ,J) = PS (NX-1,J)
	  PS (NX,J) = PS (2   ,J)
!
	  P  (1 ,J) = P  (NX-1,J)
	  P  (NX,J) = P  (2   ,J)
        END DO


      call mpi_isend( p(1,2), nx, mpi_double_precision, itop, 1, mpicom, isend, ierr)
      call mpi_irecv( p(1,ny),nx, mpi_double_precision, ibot, 1, mpicom, irecv, ierr)

!

        DO J=begj,endj
	  DO I=2,NX-1
             WS(I,J,1)=0.0D0
             WS(I,J,NZ)=0.0D0
	  END DO
	  WS(1 ,J,1) = WS(NX-1,J, 1)
	  WS(NX,J,1) = WS(2   ,J, 1)
	  WS(1,J,NZ) = WS(NX-1,J,NZ)
	  WS(NX,J,NZ)= WS(2   ,J,NZ)
	END DO


      call mpi_wait(isend,istatus,ierr)
      call mpi_wait(irecv,istatus,ierr)


!
      DO J=begj,endj
        DO I=2,NX-1
!
!         CALCULATING THE AVERAGE VALUE OF P AT V-GRID.
!







         if ((myrank.eq.0).and.(j.eq.endj)) then
            PY=0.0D0
         else
            PY=0.5D0*(P(I,J)+P(I,J+1))
         endif

!
!         CALCULATING THE AVERAGE VALUE OF P AT U-GRID.
!
          PX=0.5D0*(P(I,J)+P(I-1,J))
!
          DO K=1,NL
!
             U (I,J,K)=PX*US(I,J,K)
             V (I,J,K)=PY*VS(I,J,K)
!
!         CALCULATING TT=(TS-TB)*P*RD/CB.
!
	     TT (I,J,K)=(TS(I,J,K)-TB(I,J,K))*P(I,J)*RD/CB(I,J,K)
!
          ENDDO
        ENDDO
!
	  DO K=1,NL
!
	    US (1 ,J,K) = US (NX-1,J,K)
	    US (NX,J,K) = US (2   ,J,K)
!
	    VS (1 ,J,K) = VS (NX-1,J,K)
	    VS (NX,J,K) = VS (2   ,J,K)
!
	    TS (1 ,J,K) = TS (NX-1,J,K)
	    TS (NX,J,K) = TS (2   ,J,K)
!
	    U  (1 ,J,K) = U  (NX-1,J,K)
	    U  (NX,J,K) = U  (2   ,J,K)
!
	    V  (1 ,J,K) = V  (NX-1,J,K)
	    V  (NX,J,K) = V  (2   ,J,K)
!
	    TT (1 ,J,K) = TT (NX-1,J,K)
	    TT (NX,J,K) = TT (2   ,J,K)
!
	  END DO
      ENDDO
!
	ELSE
!
        DO J=begj,endj
	  DO I=2,NX-1
	     P(I,J)=SQRT(PS(I,J))
	  END DO
!
	  PS (1 ,J) = PS (NX-1,J)
	  PS (NX,J) = PS (2   ,J)
!
	  P  (1 ,J) = P  (NX-1,J)
	  P  (NX,J) = P  (2   ,J)
!
	END DO


      call mpi_isend( p(1,2), nx, mpi_double_precision, itop, 1, mpicom, isend, ierr)
      call mpi_irecv( p(1,ny),nx, mpi_double_precision, ibot, 1, mpicom, irecv, ierr)
      call mpi_wait(isend,istatus,ierr)
      call mpi_wait(irecv,istatus,ierr)


!
      DO J=begj,endj
        DO I=2,NX-1
!
!         CALCULATING THE AVERAGE VALUE OF P AT V-GRID.
!







         if ((myrank.eq.0).and.(j.eq.endj)) then
            PY=0.0D0
         else
            PY=0.5D0*(P(I,J)+P(I,J+1))
         endif

!
!         CALCULATING THE AVERAGE VALUE OF P AT U-GRID.
!
          PX=0.5D0*(P(I,J)+P(I-1,J))
!
          DO K=1,NL
!
          U (I,J,K)=PX*US(I,J,K)
          V (I,J,K)=PY*VS(I,J,K)
!
!         CALCULATING TT=(TS-TB)*P*RD/CB.
!
	    TT (I,J,K)=(TS(I,J,K)-TB(I,J,K))*P(I,J)*RD/CB(I,J,K)
!
          ENDDO
        ENDDO
!
	  DO K=1,NL
!
	    US (1 ,J,K) = US (NX-1,J,K)
	    US (NX,J,K) = US (2   ,J,K)
!
	    VS (1 ,J,K) = VS (NX-1,J,K)
	    VS (NX,J,K) = VS (2   ,J,K)
!
	    TS (1 ,J,K) = TS (NX-1,J,K)
	    TS (NX,J,K) = TS (2   ,J,K)
!
	    U  (1 ,J,K) = U  (NX-1,J,K)
	    U  (NX,J,K) = U  (2   ,J,K)
!
	    V  (1 ,J,K) = V  (NX-1,J,K)
	    V  (NX,J,K) = V  (2   ,J,K)
!
	    TT (1 ,J,K) = TT (NX-1,J,K)
	    TT (NX,J,K) = TT (2   ,J,K)
!
	  END DO
      ENDDO
!
	END IF
!- check ---------------------------------------------------------
!
!#if (defined )
!      write(filename,14) 'iaptrsf-p-',myrank,'.out'
!14    format(a10,i1,a4)
!      open (10,file=trim(filename))
!#else
!      open (10,file='iaptrsf-s.out')
!#endif
!
!      write(10,*) '------------------- u -----------------'
!      write(10,11) (j,(u(i,j,10),i=1,2),j=1,ny)
!
!      write(10,*) '------------------- v -----------------'
!      write(10,11) (j,(v(i,j,10),i=1,2),j=1,ny)
!
!      write(10,*) '------------------- tt -----------------'
!      write(10,11) (j,(tt(i,j,10),i=1,2),j=1,ny)
!
!      write(10,*) '------------------- p -----------------'
!      write(10,11) (j,(p(i,j),i=1,2),j=1,ny)
!
!      write(10,*) '------------------- pps -----------------'
!      write(10,11) (j,(pps(i,j),i=1,2),j=1,ny)
!
!11    format(1x,i5,2e30.20)
!
!
!      close (10)
!
!#if (defined )
!!      call mpi_finalize(j)
!#endif
!!      stop 'iaptrsf'
!
!!-----------------------------------------------------------------

!
	RETURN
	END
