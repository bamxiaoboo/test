# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIFUVT.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIFUVT.F"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIFUVT.F" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIFUVT.F" 2

!!(2003.11.10)
!!---------------------


      SUBROUTINE DIFUVT(U,US,V,VS,WS,P,PS,PLY,DPS,TT,H,HPS,CB,DCB
     _                ,SIGL,DSIG,DY,OUX,OVX,OUY,OVY,SINV,FF
     _                ,CUR,DLT1,DLT2,MP1,MP2,MP3,MM1,MM2,MM3,WTGV
     _                ,DU,DV,DTT,SU,SV,ST,FBC)


      use mpishorthand, only: mpicom


      IMPLICIT NONE
!
!	This subroutine is to calculate the tendency of the wind and the temperature:
!     DU, DV, DTT


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
# 23 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIFUVT.F" 2



# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/commpi.h" 1

        
      integer nprocs, myrank, ierr, itop, ibot, jbeg, jend, jpole
      logical inc_pole

      common/commpi/ nprocs, myrank, itop, ibot, jbeg, jend, jpole, inc_pole
# 26 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIFUVT.F" 2

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
# 27 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIFUVT.F" 2

      character*50 filename

      integer num1,num2
      parameter(num1=nx*(nl*4+1),num2=nx*(8*nl+2*nz+1) )

      real*8  worksb( num1 )
      real*8  workrt( num1 )
      real*8  workst( num2 )
      real*8  workrb( num2 )

      integer isend1,irecv1
      integer isend2,irecv2
      integer istatus(mpi_status_size)
      integer ii



!
!	The file PARA is to define the parameters related to the model resolution:
!     NX is the grid number in longitude
!     NY is the grid number in latitude
!     NL is the number of vertical layers
!
	  REAL*8
     _       U  (NX,NY,NL)  !  input variable, U  = u*sqrt(Ps)
     _	  ,US(NX,NY,NL )  !  input variable, US = u, zonal wind
     _      ,V  (NX,NY,NL)  !	 input variable, V  = v*sqrt(Ps)
     _      ,VS(NX,NY,NL )  !  input variable, VS = v, meridional wind
     _      ,WS(NX,NY,NZ )  !  input variable, WS = w, vertical velocity
     _      ,P  (NX,NY   )  !  input variable, P  = sqrt(Ps)
     _      ,PS (NX,NY   )  !	 input variable, Surface pressure
     _      ,PLY(NX,NY,NL)  !  input variable, PLY=p, Pressure in Sigma Layer
     _      ,DPS(NX,NY   )  !  input variable,
!                              the tendency of the surface pressure
     _      ,TT (NX,NY,NL)  !  input variable, TT=R*T'*Sqrt(Ps)/CB,
!                              where T'=T-TB, T is the temperatur,
!                              TBB	is Temperature of the standard atmosphere
     _	  ,H (NX,NY,NZ )  !  input variable,
     _	  ,HH(NX,NY,NZ )  !  input variable,
!  			     H=gz-HBB, gz is the geopotential height,
!                              HBB is the geopotential height of the standard atmopshere
     _      ,CB (NX,NY,NL)  !  input variable, CB=Sqrt(R*(KK*TBB-dTB/dlnp)),
!                              where, KK=R/Cp, R is a constant
     _      ,DCB(NX,NY,NL)  !  input variable,
     _      ,SIGL(NL     )  !  input constant, the vertical layers
     _      ,DSIG(NL     )  !  input constant, the vertical stepsizes
     _      ,DY             !  input constant,
!                              the horizontal stepsize in meridional direction
     _      ,SINV(NY)       !  input constant, sin(theta) at half grid j+1/2
     _      ,OUX(NY)        !  input constant, OUX=1/(RAD*SINU*DX*MDJ)
!                              where, DX is the horizontal stepsize in zonal direction,
!                              MDJ is the leaping length of the central difference
!                              SINU is sin(theta) at intger grid j
     _      ,OVX(NY)        !  input constant, OUX=1/(RAD*SINV*DX*MDJ)
     _      ,OUY(NY)        !  input constant, OUY=1/(RAD*SINU*DY*WTGU)
!                              where WTGU is the weighting at the integer grid j
     _      ,OVY(NY)        !  input constant, OUY=1/(RAD*SINV*DY*WTGU)
     _      ,WTGV(NY)       !	 input constant,
!                              the weighting at the half grid j+1/2,
     _	  ,DLT1           !  input constant
     _      ,DLT2           !	 input constant
     _      ,FF(NY)         !	 input constant
     _      ,CUR(NY)		  !	 input constant
!
	INTEGER
     _       MM1(NX,NY)     !	 input constant
     _      ,MP1(NX,NY)     !	 input constant
     _      ,MM2(NX,NY)     !	 input constant
     _      ,MP2(NX,NY)     !	 input constant
     _      ,MM3(NX,NY)     !	 input constant
     _      ,MP3(NX,NY)     !	 input constant
!
      REAL*8
     _       DU(NX,NY,NL)	  !  output variables
     _      ,DV(NX,NY,NL)   !  output variables
     _      ,DTT(NX,NY,NL)  !  output variables
     _      ,SU(NX,NY,NL)   !  input variables
     _      ,SV(NX,NY,NL)   !  input variables
     _      ,ST(NX,NY,NL)   !  input variables
!
      REAL*8  TTZ(NX,NY,NZ+1)
      REAL*8  UZ(NX,NY,NZ+1),VZ(NX,NY,NZ+1)
      INTEGER I,J,K
      INTEGER IM1,IP2,IP1,IM2,I1,J0,IM3,IP3
      REAL*8  OY1,R22,R21,R12,DYP0,DXP1,DXP0,DTTP,TO3P
      REAL*8  R11,OUX2,DYP2,O2,O1,TL3,TO3,TO2,TO1
      REAL*8  TL2,TL1,OPK0,OZ2
      REAL*8  OY4,RI0,TO1P,TL3P,TO2P
      REAL*8  O2P,TLP,O1P,WKP
      REAL*8  RV1,RV2,OVX4,OVY4,PDXP1,PDXP2,R14
      REAL*8  OUX4,R10,R20,R24,UL2,UL3
      REAL*8  H0,UL1,PX2,FSV,PX1,AXP0
      REAL*8  OPK1,PDYP2,AYP0,PDYP1
      REAL*8  VL2,VL3,OPK3,VL1,PY1
      REAL*8  FSU,PY2,FFYY,OZ4,OPK2
      REAL*8  FS0,FFXX,H1,OYY4,HPS(NX,NY)
      REAL*8  FBC(NX,NY,NL),TTV(NX,NY,NL)
      integer begj,endj


      begj = 2
      endj = ny-1







      ii=1
      do i=1,nx
         worksb(ii)=p(i,ny-1)
         ii=ii+1
      enddo
!--1--
      do k=1,nl
       do i=1,nx
         worksb(ii)=u(i,ny-1,k)
         ii=ii+1
       enddo
      enddo
!--2--
      do k=1,nl
       do i=1,nx
         worksb(ii)=v(i,ny-1,k)
         ii=ii+1
       enddo
      enddo
!--3--
      do k=1,nl
       do i=1,nx
         worksb(ii)=tt(i,ny-1,k)
         ii=ii+1
       enddo
      enddo
!--4--
      do k=1,nl
       do i=1,nx
         worksb(ii)=vs(i,ny-1,k)
         ii=ii+1
       enddo
      enddo

      call mpi_isend( worksb,num1,mpi_double_precision,ibot,1,mpicom,isend1,ierr)
      call mpi_irecv( workrt,num1,mpi_double_precision,itop,1,mpicom,irecv1,ierr)


C     AS FOLLOWS, THE IMPROVED LEAP-FROG AND THE REGENERATION OF
C     VARIABLES WILL BE FINISHED IN THE SAME CYCLES.
C
      I1=NX-1
      J0=NY-1
      RI0=DFLOAT(NX-2)
      OY1=1.0D0/(RAD*DY)
      OY4=0.25D0*OY1

C
C     CALCULATING DU/DT, DV/DT AND DTT/DT.
C
C     FORMING THE FACTORS OUT OF ALL THE CYCLES AND CLEANING SOME
C     SPECIAL ARRAIES.
C
!     (DO J=1,NY)
      DO J=begj,endj
      DO I=1,NX
      DO K=1,NL
	 TTV(I,J,K)=TT(I,J,K)*FBC(I,J,K)
      END DO
      END DO
      END DO
c
C
        DO K=1,NZ
        DO J=begj,endj
        DO I=1,NX
           HH(I,J,K)=H(I,J,K)-HPS(I,J)
        ENDDO
        ENDDO
        ENDDO

!     (DO J=1,NY)
      do j=begj,endj
        DO K = 1,NZ+1
          IF(K.EQ.1.OR.K.EQ.NZ+1) THEN
            DO I = 1,NX
              TTZ(I,J,K) = 0.0D0
              UZ(I,J,K) = 0.0D0
              VZ(I,J,K) = 0.0D0
            ENDDO
          ELSE
            DO I = 1,NX
              TTZ(I,J,K) = TT(I,J,K-1)
              UZ(I,J,K) = U(I,J,K-1)
              VZ(I,J,K) = V(I,J,K-1)
            ENDDO
          ENDIF
        ENDDO
      enddo


      ii=1
      do i=1,nx
         workst(ii)=p(i,2)
         ii=ii+1
      enddo
!--1--
      do k=1,nz
       do i=1,nx
         workst(ii)=h(i,2,k)
         ii=ii+1
       enddo
      enddo
!--2--
      do k=1,nz
       do i=1,nx
         workst(ii)=ws(i,2,k)
         ii=ii+1
       enddo
      enddo
!--1--
      do k=1,nl
       do i=1,nx
         workst(ii)=cb(i,2,k)
         ii=ii+1
       enddo
      enddo
!--2--
      do k=1,nl
       do i=1,nx
         workst(ii)=ply(i,2,k)
         ii=ii+1
       enddo
      enddo
!--3--
      do k=1,nl
       do i=1,nx
         workst(ii)=u(i,2,k)
         ii=ii+1
       enddo
      enddo
!--4--
      do k=1,nl
       do i=1,nx
         workst(ii)=v(i,2,k)
         ii=ii+1
       enddo
      enddo
!--5--
      do k=1,nl
       do i=1,nx
         workst(ii)=tt(i,2,k)
         ii=ii+1
       enddo
      enddo
!--6--
      do k=1,nl
       do i=1,nx
         workst(ii)=ttv(i,2,k)
         ii=ii+1
       enddo
      enddo
!--7--
      do k=1,nl
       do i=1,nx
         workst(ii)=us(i,2,k)
         ii=ii+1
       enddo
      enddo
!--8--
      do k=1,nl
       do i=1,nx
         workst(ii)=vs(i,2,k)
         ii=ii+1
       enddo
      enddo

      call mpi_isend( workst,num2,mpi_double_precision,itop,2,mpicom,isend2,ierr)
      call mpi_irecv( workrb,num2,mpi_double_precision,ibot,2,mpicom,irecv2,ierr)

!----------------------------------------------unpacking dataset 1----
      call mpi_wait( isend1,istatus,ierr )
      call mpi_wait( irecv1,istatus,ierr )

      ii=1
      do i=1,nx
         p(i,1)=workrt(ii)
         ii=ii+1
      enddo
!--1--
      do k=1,nl
       do i=1,nx
         u(i,1,k)=workrt(ii)
         ii=ii+1
       enddo
      enddo
!--2--
      do k=1,nl
       do i=1,nx
         v(i,1,k)=workrt(ii)
         ii=ii+1
       enddo
      enddo
!--3--
      do k=1,nl
       do i=1,nx
         tt(i,1,k)=workrt(ii)
         ii=ii+1
       enddo
      enddo
!--4--
      do k=1,nl
       do i=1,nx
         vs(i,1,k)=workrt(ii)
         ii=ii+1
       enddo
      enddo

!----------------------------------------------unpacking dataset 2----

      call mpi_wait( isend2,istatus,ierr )
      call mpi_wait( irecv2,istatus,ierr )

      ii=1
      do i=1,nx
         p(i,ny)=workrb(ii)
         ii=ii+1
      enddo
!--1--
      do k=1,nz
       do i=1,nx
         h(i,ny,k)=workrb(ii)
         ii=ii+1
       enddo
      enddo
!--2--
      do k=1,nz
       do i=1,nx
         ws(i,ny,k)=workrb(ii)
         ii=ii+1
       enddo
      enddo
!--1--
      do k=1,nl
       do i=1,nx
         cb(i,ny,k)=workrb(ii)
         ii=ii+1
       enddo
      enddo
!--2--
      do k=1,nl
       do i=1,nx
         ply(i,ny,k)=workrb(ii)
         ii=ii+1
       enddo
      enddo
!--3--
      do k=1,nl
       do i=1,nx
         u(i,ny,k)=workrb(ii)
         ii=ii+1
       enddo
      enddo
!--4--
      do k=1,nl
       do i=1,nx
         v(i,ny,k)=workrb(ii)
         ii=ii+1
       enddo
      enddo
!--5--
      do k=1,nl
       do i=1,nx
         tt(i,ny,k)=workrb(ii)
         ii=ii+1
       enddo
      enddo
!--6--
      do k=1,nl
       do i=1,nx
         ttv(i,ny,k)=workrb(ii)
         ii=ii+1
       enddo
      enddo
!--7--
      do k=1,nl
       do i=1,nx
         us(i,ny,k)=workrb(ii)
         ii=ii+1
       enddo
      enddo
!--8--
      do k=1,nl
       do i=1,nx
         vs(i,ny,k)=workrb(ii)
         ii=ii+1
       enddo
      enddo




C
C     CALCULATING DTT/DT AT POLES.
C



       if (myrank.eq.(nprocs-1)) then

!!        (IF(J.EQ.1) THEN)   ! for the north pole

          DO K=1,NL
            OZ2=0.5D0/DSIG(K)
            OZ4=0.5D0*OZ2
C
C       CALCULATING ADVECTION OF TT AND VERTICAL MOTION OVER PRESSURE
C       AT POLES.
C
            TLP=0.0D0
            O2P=0.0D0
            DO I=2,I1
              TLP=TLP+VS(I,jpole,K)*TT(I,jpole+1,K)
              O2P=O2P+(P(I,jpole+1)-P(1,jpole))*V(I,jpole,K)
            ENDDO
            WKP=2.0D0/RI0*OY1*WTGV(jpole)
            TLP=WKP*TLP
            O2P=2.0D0*WKP*O2P*SIGL(K)/PLY(1,jpole,K)
            O1P=(0.5D0*PS(1,jpole)*(WS(1,jpole,K+1)+WS(1,jpole,K))
     &        +DPS(1,jpole)*SIGL(K))/PLY(1,jpole,K)
            TL3P=OZ2*(WS(1,jpole,K+1)*TTZ(1,jpole,K+2)
     &               -WS(1,jpole,K)*TTZ(1,jpole,K))
C
            TO1P=CB(1,jpole,K)*P(1,jpole)*O1P
            TO2P=CB(1,jpole,K)*P(1,jpole)*O2P
            TO3P=(DLT1*CAPA-DLT2*DCB(1,jpole,K))*TTV(1,jpole,K)*(O1P+O2P)
C
C     CALCULATING DTT/DT AND FORMING BOTH POLAR BOUNDARIES.
            DTTP=-TLP-TL3P+TO1P+TO2P+TO3P
            DO I=1,NX
              DTT(I,jpole,K)=DTTP+ST(I,jpole,K)
              DU(I,jpole,K)=SU(I,jpole,K)
            ENDDO
            DO I=2,I1
              IP1=MP1(I,jpole)
              IM1=MM1(I,jpole)
              IP2=MP2(I,jpole)
              IM2=MM2(I,jpole)
C
              DYP0=P(I,jpole+1)-P(I,jpole)
              PDYP1=P(I,jpole+1)*DYP0
              PDYP2=P(I,jpole)*DYP0
              AYP0=0.25D0*(P(I,jpole)+P(I,jpole+1))
C
              OPK0=1.0D0/PLY(I,jpole,K)
              OPK3=1.0D0/PLY(I,jpole+1,K)
C
              VL1=0.25D0*OVX(jpole)*(US(IP2,jpole+1,K)*V(IP1,jpole,K)
     &                          -US(IM2,jpole+1,K)*V(IM1,jpole,K))
              VL2=0.25D0*OVY(jpole)*(SINV(jpole+1)*VS(I,jpole+1,K)
     &                          +SINV(jpole)*VS(I,jpole,K))*V(I,jpole+1,K)
              VL3=OZ4*((WS(I,jpole+1,K+1)+WS(I,jpole,K+1))*VZ(I,jpole,K+2)
     &                -(WS(I,jpole+1,K)+WS(I,jpole,K))*VZ(I,jpole,K))
C
              FSU=0.25D0*((FF(jpole+1)+CUR(jpole+1)*US(I,jpole+1,K))*U(I,jpole+1,K)
     &                   +(FF(jpole+1)+CUR(jpole+1)*US(I+1,jpole+1,K))*U(I+1,jpole+1,K))
              PY1=OY1*AYP0*(H(I,jpole+1,K+1)+H(I,jpole+1,K)-H(I,jpole,K+1)-H(I,jpole,K))
              PY2=OY1*(PDYP1*TTV(I,jpole+1,K)*OPK3*CB(I,jpole+1,K)
     &                +PDYP2*TTV(I,jpole,  K)*OPK0*CB(I,jpole,  K))*SIGL(K)
C
              FFYY=PY1+PY2
              DV(I,jpole,K)=-VL1-VL2-VL3+FSU-FFYY*WTGV(jpole)+SV(I,jpole,K)

C
            ENDDO
            DV(1,jpole,K)=DV(I1,jpole,K)
            DV(NX,jpole,K)=DV(2,jpole,K)
          ENDDO

       ENDIF






       if (myrank.eq.0) then

!!        (ELSE IF(J.EQ.NY) THEN)  ! for the south pole
          DO K=1,NL
            OZ2=0.5D0/DSIG(K)
            OZ4=0.5D0*OZ2
C
C       CALCULATING ADVECTION OF TT AND VERTICAL MOTION OVER PRESSURE
C       AT POLES.
C
            TLP=0.0D0
            O2P=0.0D0
            DO I=2,I1
              TLP=TLP+VS(I,jpole-1,K)*TT(I,jpole-1,K)
              O2P=O2P+(P(1,jpole)-P(I,jpole-1))*V(I,jpole-1,K)
            ENDDO
            WKP=2.0D0/RI0*OY1*WTGV(jpole-1)
            TLP=-WKP*TLP
            O2P=2.0D0*WKP*O2P*SIGL(K)/PLY(1,jpole,K)
            O1P=(0.5D0*PS(1,jpole)*(WS(1,jpole,K+1)+WS(1,jpole,K))
     &        +DPS(1,jpole)*SIGL(K))/PLY(1,jpole,K)
            TL3P=OZ2*(WS(1,jpole,K+1)*TTZ(1,jpole,K+2)
     &             -WS(1,jpole,K)*TTZ(1,jpole,K))
C
            TO1P=CB(1,jpole,K)*P(1,jpole)*O1P
            TO2P=CB(1,jpole,K)*P(1,jpole)*O2P
            TO3P=(DLT1*CAPA-DLT2*DCB(1,jpole,K))*TTV(1,jpole,K)*(O1P+O2P)
C
C     CALCULATING DTT/DT AND FORMING BOTH POLAR BOUNDARIES.
            DTTP=-TLP-TL3P+TO1P+TO2P+TO3P
            DO I=1,NX
              DTT(I,jpole,K)=DTTP+ST(I,jpole,K)
              DU(I,jpole,K)=SU(I,jpole,K)
              DV(I,jpole,K)=SV(I,jpole,K)
            ENDDO
          ENDDO
!!        ELSE

       endif

       do j=jbeg,jend



C
C     CALCULATING DTT/DT FROM J=2 TO J=J0.
C
C     FORMING THE FACTORS INDEPENDENT ON I, K
C
          OUX2=0.5D0*OUX(J)
          R11=SINV(J)*OUY(J)
          R12=0.5D0*R11
          R21=SINV(J-1)*OUY(J)
          R22=0.5D0*R21
          OUX4=0.5D0*OUX2
          R14=0.5D0*R12
          R24=0.5D0*R22
          R10=R14/(OY1*WTGV(J))
          R20=R24/(OY1*WTGV(J-1))
          OVX4=0.25D0*OVX(J)
          OVY4=0.25D0*OVY(J)
          RV1=SINV(J+1)*OVY4
          RV2=SINV(J-1)*OVY4
	  OYY4=OY4*WTGV(J)
C
          DO K=1,NL
            OZ2=0.5D0/DSIG(K)
            OZ4=0.5D0*OZ2
C
            DO I=2,I1
C
              IP1=MP1(I,J)
              IM1=MM1(I,J)
              IP2=MP2(I,J)
              IM2=MM2(I,J)
C
C     FORMING THE FACTORS INDEPENDENT ON K.
C
              DXP0=P(I,J)-P(IM1,J)
              DXP1=P(IP1,J)-P(I,J)
              DYP0=P(I,J+1)-P(I,J)
              DYP2=P(I,J)-P(I,J-1)
C
C     TAKING THE ARRAY ELEMENTS APPEARING IN FOLLOWING FORMULAS
C     REPEATEDLY AND PLACING THEM INTO WORKING UNITS.
C
              OPK0=1.0D0/PLY(I,J,K)
C
C     CALCULATING DTT/DT FOR J=2--J0, I=2--I1 ANDK=1--NL.
C
              TL1=OUX2*(US(IP2,J,K)*TT(IP1,J,K)-US(IM2,J,K)*TT(IM1,J,K))
              TL2=R12*VS(I,J,K)*TT(I,J+1,K)-R22*VS(I,J-1,K)*TT(I,J-1,K)
              TL3=OZ2*(WS(I,J,K+1)*TTZ(I,J,K+2)-WS(I,J,K)*TTZ(I,J,K))
              O1=OPK0*(0.5D0*PS(I,J)*(WS(I,J,K+1)+WS(I,J,K))
     &          +DPS(I,J)*SIGL(K))
              O2=OPK0*SIGL(K)*(OUX(J)*(DXP1*U(IP2,J,K)+DXP0*U(IM2,J,K))
     &                  +R11*DYP0*V(I,J,K)+R21*DYP2*V(I,J-1,K))
              TO1=CB(I,J,K)*P(I,J)*O1
              TO2=CB(I,J,K)*P(I,J)*O2

              TO3=(DLT1*CAPA-DLT2*DCB(I,J,K))*TTV(I,J,K)*(O1+O2)
              DTT(I,J,K)=-TL1-TL2-TL3+TO1+TO2+TO3+ST(I,J,K)
            ENDDO
            DTT(1,J,K)=DTT(I1,J,K)
            DTT(NX,J,K)=DTT(2,J,K)
C
            DO I=2,I1
C
              IP1=MP1(I,J)
              IM1=MM1(I,J)
              IP2=MP2(I,J)
              IM2=MM2(I,J)
              IP3=MP3(I,J)
              IM3=MM3(I,J)
C
C     FORMING THE FACTORS INDEPENDENT ON K.
C
              DXP0=P(IP3,J)-P(IM3,J)
              PDXP1=P(IP3,J)*DXP0
              PDXP2=P(IM3,J)*DXP0
              DXP1=P(IP2,J)-P(IM2,J)
              AXP0=0.25D0*(P(IP3,J)+P(IM3,J))
              DYP0=P(I,J+1)-P(I,J)
              PDYP1=P(I,J+1)*DYP0
              PDYP2=P(I,J)*DYP0
              DYP2=P(I,J)-P(I,J-1)
              AYP0=0.25D0*(P(I,J+1)+P(I,J))
C
C     TAKING THE ARRAY ELEMENTS APPEARING IN FOLLOWING FORMULAS
C     REPEATEDLY AND PLACING THEM INTO WORKING UNITS.
C
              OPK0=1.0D0/PLY(I,J,K)
              OPK1=1.0D0/PLY(IP3,J,K)
              OPK2=1.0D0/PLY(IM3,J,K)
              OPK3=1.0D0/PLY(I,J+1,K)
              H1=H(I,J,K+1)+H(I,J,K)
!             H0=H(IP3,J,K+1)+H(IP3,J,K)
              H0=HH(IP3,J,K+1)+HH(IP3,J,K)
C
C     CALCULATING DU/DT, DV/DT J=2--J0, I=2--I1 AND K=1--NL.
C
              UL1=OUX4*((US(I,J,K)+US(IP1,J,K))*U(IP1,J,K)
     &                 -(US(I,J,K)+US(IM1,J,K))*U(IM1,J,K))
              UL2=R14*(VS(I,J,K)+VS(I-1,J,K))*U(I,J+1,K)
     &           -R24*(VS(I,J-1,K)+VS(I-1,J-1,K))*U(I,J-1,K)
              UL3=OZ4*((WS(I,J,K+1)+WS(I-1,J,K+1))*UZ(I,J,K+2)
     &                -(WS(I,J,K)+WS(I-1,J,K))*UZ(I,J,K))
              FS0=FF(J)+CUR(J)*US(I,J,K)
              FSV=FS0*(R10*(V(I,J,K)+V(I-1,J,K))
     &                +R20*(V(I,J-1,K)+V(I-1,J-1,K)))
!             PX1=OUX(J)*(AXP0*(H0-H(IM3,J,K+1)-H(IM3,J,K)))
              PX1=OUX(J)*(AXP0*(H0-HH(IM3,J,K+1)-HH(IM3,J,K)))
              PX2=OUX(J)*(PDXP1*TTV(IP3,J,K)*OPK1*CB(IP3,J,K)
     &                   +PDXP2*TTV(IM3,J,K)*OPK2*CB(IM3,J,K))*SIGL(K)
              FFXX=PX1+PX2
C
              VL1=OVX4*((US(IP2,J+1,K)+US(IP2,J,K))*V(IP1,J,K)
     &                 -(US(IM2,J+1,K)+US(IM2,J,K))*V(IM1,J,K))
              VL2=(RV1*VS(I,J+1,K)+OYY4*VS(I,J,K))*V(I,J+1,K)
     &           -(RV2*VS(I,J-1,K)+OYY4*VS(I,J,K))*V(I,J-1,K)
              VL3=OZ4*((WS(I,J+1,K+1)+WS(I,J,K+1))*VZ(I,J,K+2)
     &                -(WS(I,J+1,K)+WS(I,J,K))*VZ(I,J,K))
              FSU=0.25D0*(FS0*U(I,J,K)
     &                   +(FF(J)+CUR(J)*US(I+1,J,K))*U(I+1,J,K)
     &                   +(FF(J+1)+CUR(J+1)*US(I,J+1,K))*U(I,J+1,K)
     &                   +(FF(J+1)+CUR(J+1)*US(I+1,J+1,K))*U(I+1,J+1,K))
              PY1=OY1*(AYP0*(H(I,J+1,K+1)+H(I,J+1,K)-H1))
              PY2=OY1*(PDYP1*TTV(I,J+1,K)*OPK3*CB(I,J+1,K)
     &                +PDYP2*TTV(I,J,K)*OPK0*CB(I,J,K))*SIGL(K)
              FFYY=PY1+PY2
C
              DU(I,J,K)=-UL1-UL2-UL3-FSV-FFXX+SU(I,J,K)
              DV(I,J,K)=-VL1-VL2-VL3+FSU-FFYY*WTGV(J)+SV(I,J,K)
            ENDDO
            DU(1,J,K)=DU(I1,J,K)
            DU(NX,J,K)=DU(2,J,K)
            DV(1,J,K)=DV(I1,J,K)
            DV(NX,J,K)=DV(2,J,K)
          ENDDO
!!        (ENDIF)
	END DO
!- check ---------------------------------------------------------
!
!#if (defined )
!      write(filename,14) 'difuvt-p-',myrank,'.out'
!14    format(a9,i1,a4)
!      open (10,file=trim(filename))
!#else
!      open (10,file='difuvt-s.out')
!#endif
!
!      write(10,*) '------------------- du 26 -----------------'
!      write(10,11) (j,(du(i,j,26),i=1,2),j=1,ny)
!
!      write(10,*) '------------------- dv 26 -----------------'
!      write(10,11) (j,(dv(i,j,26),i=1,2),j=1,ny)
!
!      write(10,*) '------------------- dtt 26 -----------------'
!      write(10,11) (j,(dtt(i,j,26),i=1,2),j=1,ny)
!
!      write(10,*) '------------------- du 12 -----------------'
!      write(10,11) (j,(du(i,j,12),i=1,2),j=1,ny)
!
!      write(10,*) '------------------- dv 12 -----------------'
!      write(10,11) (j,(dv(i,j,12),i=1,2),j=1,ny)
!
!      write(10,*) '------------------- dtt 12 -----------------'
!      write(10,11) (j,(dtt(i,j,12),i=1,2),j=1,ny)
!
!11    format(1x,i5,2e30.20)
!
!
!      close (10)
!
!#if (defined )
!!      call mpi_finalize(j)
!#endif
!!      stop 'difuvt'
!
!-----------------------------------------------------------------


!
	RETURN
	END
