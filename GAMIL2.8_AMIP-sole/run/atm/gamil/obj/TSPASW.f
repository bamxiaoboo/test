# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/TSPASW.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/TSPASW.F"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/TSPASW.F" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/TSPASW.F" 2

C     =======================
      SUBROUTINE TSPASW(Q,W,DTDSG)
C     =======================
C
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
# 11 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/TSPASW.F" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/PARADD" 1
      INTEGER IB,IE,JB,JE,KE,NM
      PARAMETER ( IB=2,IE=NX-1,JB=2,JE=NY-1,KE=NL+2,NM=NL-1 )
# 12 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/TSPASW.F" 2



# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/commpi.h" 1

        
      integer nprocs, myrank, ierr, itop, ibot, jbeg, jend, jpole
      logical inc_pole

      common/commpi/ nprocs, myrank, itop, ibot, jbeg, jend, jpole, inc_pole
# 15 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/TSPASW.F" 2

C
      REAL*8 Q(NX,NY,NL),W(NX,NY,NL)
      REAL*8 QWMIN(NX,NY,NL),QWMAX(NX,NY,NL)
!
      REAL*8 WW(NL),FW(NZ),BETAW(NL),QW(NL),QWSTAR(NL)
     _      ,WSTAR(NL),AW(NL),HS(NL),HW(NL),DTDSG(NL)
!
      REAL*8 GAMA,CWSTAR,CW,TEMP1,TEMP2,TEMP3,TEMP4
!
      REAL*8  ZERO,HALF,FOURTH,EPSM
      DATA ZERO,HALF,FOURTH,EPSM/ 0.0D0,0.5D0,0.25D0,
     $     1.0D-80/
*    $     1.0E-6/
      INTEGER I,J,K,IS,IT
      integer begj,endj


      character*50 filename

      begj=2
      endj=ny-1




!
       DO I=1,NX
       DO J=begj,endj
       DO K=1,NL
          QWMIN(I,J,K)=1.0E15
          QWMAX(I,J,K)=-1.0E15
       ENDDO
       ENDDO
       ENDDO
!
      DO K=1,NL
	HS(K)=1.0D0/DTDSG(K)
      ENDDO
C
      DO K=2,NL
	HW(K)=HALF*(HS(K)+HS(K-1))
      ENDDO
C
      DO J=begj,endj

# 75 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/TSPASW.F"
      IF(J.EQ.1) THEN
	IS=NX
	IT=NX
      ELSE IF(J.EQ.NY) THEN
	IS=1
	IT=1
      ELSE
	IS=1
	IT=NX
      ENDIF

C
      DO I=IS,IT
C
      DO K=1,NL
	QW(K)=Q(I,J,K)
	WW(K)=W(I,J,K)
      ENDDO
C
      DO K=2,NL
	FW(K)=HALF*WW(K)*(QW(K)+QW(K-1))
     $ -HALF*WW(K)*WW(K)*(QW(K)-QW(K-1))/HW(K)
      ENDDO
C
      DO K=2,NM
 	TEMP1=ABS(WW(K)/HW(K))*(1-ABS(WW(K)/HW(K)))
	TEMP2=ABS(WW(K+1)/HW(K+1))*(1-ABS(WW(K+1)/HW(K+1)))
        GAMA=MAX(TEMP1,TEMP2)
	BETAW(K)=2.0D0/(2.0D0-GAMA)
	QWSTAR(K)=QW(K)-BETAW(K)*(FW(K+1)-FW(K))*DTDSG(K)
      ENDDO
C
      QWSTAR(1)=QW(1)-BETAW(2)*FW(2)*DTDSG(1)
      QWSTAR(NL)=QW(NL)+BETAW(NM)*FW(NL)*DTDSG(NL)
C
      DO K=1,NL
	IF(K.EQ.1) THEN
	  QWMIN(I,J,K)=MIN(QW(K),QW(K+1),QWMIN(I,J,K))
	  QWMAX(I,J,K)=MAX(QW(K),QW(K+1),QWMAX(I,J,K))
CCCC	ELSE IF(J.EQ.NL) THEN
	ELSE IF(K.EQ.NL) THEN
	  QWMIN(I,J,K)=MIN(QW(K),QW(K-1),QWMIN(I,J,K))
	  QWMAX(I,J,K)=MAX(QW(K),QW(K-1),QWMAX(I,J,K))
	ELSE
          QWMIN(I,J,K)=MIN(QW(K),QW(K-1),QW(K+1),QWMIN(I,J,K))
	  QWMAX(I,J,K)=MAX(QW(K),QW(K-1),QW(K+1),QWMAX(I,J,K))
	ENDIF
	  AW(K)=(QWSTAR(K)-QWMIN(I,J,K))*(QWSTAR(K)-QWMAX(I,J,K))
      ENDDO
C
      DO K=2,NL
	TEMP1=(ABS(AW(K))+AW(K))/(AW(K)+EPSM)
	TEMP2=(ABS(AW(K-1))+AW(K-1))/(AW(K-1)+EPSM)
	TEMP3=(ABS(AW(K))+AW(K))*(ABS(AW(K-1))+AW(K-1))
	TEMP4=ABS(AW(K))*ABS(AW(K-1))+EPSM
	CWSTAR=HALF*(TEMP1+TEMP2)-FOURTH*TEMP3/TEMP4
	CW=CWSTAR+(1-CWSTAR)*ABS(WW(K)/HW(K))
	WSTAR(K)=CW*WW(K)
      ENDDO
C
      DO K=2,NL
	FW(K)=HALF*WW(K)*(QW(K)+QW(K-1))
     $ -HALF*ABS(WSTAR(K))*(QW(K)-QW(K-1))
      ENDDO
C
      FW(1)=ZERO
      FW(NZ)=ZERO
C
      DO K=1,NL
	QW(K)=QW(K)-(FW(K+1)-FW(K))*DTDSG(K)
      ENDDO
C
      DO K=1,NL
	Q(I,J,K)=QW(K)
      ENDDO
C
      ENDDO



!      IF(J.EQ.1) THEN
      if (myrank.eq.nprocs-1.and.j.eq.jpole) then
        DO K = 1 ,NL
          DO I = 1 ,NX
            Q(I,J,K)= Q(NX,J,K)
          ENDDO
        ENDDO
!      ELSE IF(J.EQ.NY) THEN
      else if (myrank.eq.0.and.j.eq.jpole) then
        DO K = 1 ,NL
          DO I = 1 ,NX
            Q(I,J,K)= Q(1,J,K)
          ENDDO
        ENDDO
      ENDIF
# 186 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/TSPASW.F"
C
      ENDDO


!- check ---------------------------------------------------------
!#if (defined )
!
!      write(filename,14) 'tspasw-p-',myrank,'.out'
!14    format(a9,i1,a4)
!
!      open (10,file=trim(filename))
!
!#else
!      open (10,file='tspasw-s.out')
!#endif
!
!      write(10,*) 'qqqqqqqq--------------tspasw'
!      do j=1,ny
!        write(10,11) j,q(1,j,10),q(2,j,10)
!      enddo
!
!11    format(1x,i5,2e30.20)
!      close (10)
!
!#if (defined )
!      call mpi_finalize(j)
!#endif
!      stop'tspasw'
!--------------------------------------------------------------

      RETURN
      END

