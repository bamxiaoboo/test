# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/AVNEGQ.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/AVNEGQ.F"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/AVNEGQ.F" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/AVNEGQ.F" 2

!!(2003.11.12)
!!--------------------

      SUBROUTINE AVNEGQ(Q,DSGHL)
C     **********************
C     **********************
C
C     AVOIDE   NEGATIVE MIXING RATIO
C
      IMPLICIT NONE


# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/PARADYN" 1



!     Define the parameters related to the model resolution


      integer nprocessor
      parameter(nprocessor=4)            !by LPF


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
# 16 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/AVNEGQ.F" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/PARADD" 1
      INTEGER IB,IE,JB,JE,KE,NM
      PARAMETER ( IB=2,IE=NX-1,JB=2,JE=NY-1,KE=NL+2,NM=NL-1 )
# 17 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/AVNEGQ.F" 2



# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/commpi.h" 1

        
      integer nprocs, myrank, ierr, itop, ibot, jbeg, jend, jpole
      logical inc_pole

      common/commpi/ nprocs, myrank, itop, ibot, jbeg, jend, jpole, inc_pole
# 20 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/AVNEGQ.F" 2
      character*50 filename


      REAL*8  DSGHL(NL)
      REAL*8  Q(NX,NY,NL),QR(NL),QI
      REAL*8 ZERO
      DATA    ZERO / 0.0E0 /
      INTEGER I,J,K


      do 20  j  = jbeg,jend



      DO 20  I  = IB,IE
      DO 10  K  = 1 ,NL
10    QR(K)     = Q(I,J,K)
      DO 30  K  = 2 ,NL
      QI        = QR(K-1)
      IF( QI.LT.ZERO ) THEN
        QR(K-1)  = ZERO
        QR(K )  = QR(K ) + QI*DSGHL(K)
      ENDIF
30    CONTINUE
      IF( QR(NL).LT.ZERO ) QR(NL) = ZERO
      DO 20  K  = 1 ,NL
      Q(I,J,K)  = QR(K)
20    CONTINUE


      if (inc_pole) then
         DO 40  K  = 1 ,NL
40       QR(K)     = Q(IB,Jpole,K)
         DO 60  K  = 2 ,NL
         QI        = QR(K-1)
         IF( QI.LT.ZERO ) THEN
           QR(K-1)  = ZERO
           QR(K )  = QR(K ) + QI*DSGHL(K)
         ENDIF
60       CONTINUE
         IF( QR(NL).LT.ZERO ) QR(NL) = ZERO
         DO 50  K  = 1 ,NL
         DO 50  I  = IB,IE
         Q(I,Jpole,K)  = QR(K)
50       CONTINUE
      endif
# 83 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/AVNEGQ.F"
C
      DO 70  K  = 1 ,NL
      DO 70  J  = 1 ,NY
      Q(1 ,J,K) = Q(IE,J,K)
      Q(NX,J,K) = Q(IB,J,K)
70    CONTINUE

!- check ---------------------------------------------------------
!
!#if (defined )
!      write(filename,14) 'avnegq-p-',myrank,'.out'
!14    format(a9,i1,a4)
!
!      open (10,file=trim(filename))
!#else
!      open (10,file='avnegq-s.out')
!#endif
!
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
!      stop 'avnegq'
!!--------------------------------------------------------------


      RETURN
      END

