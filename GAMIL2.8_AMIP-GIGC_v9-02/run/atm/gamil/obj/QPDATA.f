# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/QPDATA.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/QPDATA.F"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/QPDATA.F" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/QPDATA.F" 2

!!(2003.11.11)
!!-------------------


       SUBROUTINE QPDATA2(QT,U0,V0,W0,DSGHL
     _                 ,U,V,WS,NONOS,IORD,ISOR,IP,EP
     _                 ,DSNP,DSSP,DTDLN,DTDLT,GC,DTDSG)
!     ********************************
!     ********************************
!
!     PREDICT POSITIVE DEFINITE FIELD Q        DUE TO 3-D ADVECTION
!           1)  BY USING THE SCHEME     PROPOSED BY R.C.Yu
!           2)  BY USING THE SCHEME     PROPOSED BY P.K.Smolarkiewicz
!
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
# 21 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/QPDATA.F" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/PARADD" 1
      INTEGER IB,IE,JB,JE,KE,NM
      PARAMETER ( IB=2,IE=NX-1,JB=2,JE=NY-1,KE=NL+2,NM=NL-1 )
# 22 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/QPDATA.F" 2

C
      REAL*8 U0(NX,NY,NL),V0(NX,NY,NL),W0(NX,NY,NL)
      REAL*8 U(NX,NY,NL),V(NX,NY,NL),WS(NX,NY,NZ)
      REAL*8 QT(NX,NY,NL),DQ(NX,NY,NL)
      REAL*8 DSGHL(NL)
!
      INTEGER NONOS,IORD,ISOR,IP(NX)
      REAL*8 EP,DSNP,DSSP,DTDLN(NY),DTDLT(NY),GC(NY),DTDSG(NL)
      REAL*8 UQ(NX,NY,NL),VQ(NX,NY,NL),WQ(NX,NY,NL),PQ(NX,NY)
      REAL*8 HALF,ONE
      DATA    HALF,ONE / 0.5E0,1.0E0 /
      INTEGER I,J,K
      integer begj,endj


      begj = 2
      endj = ny-1




!
!     GET THE ADVECTION VELOCITY
!
      DO K   = 1 ,NL
!        (DO J   = 1 ,NY)
          do j   = begj,endj
            DO I   = 1 ,NX
               UQ(I,J,K)  = HALF*(U (I,J,K)+U0(I,J,K))
               VQ(I,J,K)  = HALF*(V (I,J,K)+V0(I,J,K))
               WQ(I,J,K)  = HALF*(WS(I,J,K)+W0(I,J,K))
            ENDDO
         ENDDO
!
!     SAVE THE FIELD ON INPUT
!
!        (DO J   = 1 ,NY)
         do j   = begj,endj
            DO I   = 1 ,NX
               DQ(I,J,K)    = QT(I,J,K)
            ENDDO
         ENDDO
      ENDDO
!
!     PERFORM HORIZONTAL ADVECTION IN SPHERICAL GEOMETRY
!
!    (DO J   = 1 ,NY)
      do j   = begj,endj
         DO I = 1 ,NX
            PQ(I,J)    = ONE
         ENDDO
      ENDDO
!
!     DO THE 2-D ADVECTION BY MPDATA
!
      CALL MPDATA(QT,UQ,VQ,PQ,DSNP,DSSP,GC,DTDLT,DTDLN
     _           ,EP,NONOS,IORD,ISOR,IP)

!      write(*,*) 'mpdata called'
!
!     PERFORM THE VERTICAL ADVECTION
!       BY  P.K.Smolarkiewicz
      CALL VPDATA(QT,WQ,DTDSG,EP,NONOS,ISOR,IORD)

!      write(*,*) 'vpdata called'
!
!     PERFORM VERTICAL REDISTRIBUTION TO AVOID NEGATIVE Q-H2O
!
      CALL AVNEGQ(QT,DSGHL)

!      write(*,*) 'avnegq called'
      RETURN
      END

!##################################################################################
!! (2003.11.30)

      SUBROUTINE QPDATA1(QT,U0,V0,W0,DSGHL
     _                 ,U,V,WS
     _                 ,DSNP,DSSP,DTDLN,DTDLT,GC,DTDSG)

!
!     PREDICT POSITIVE DEFINITE FIELD Q        DUE TO 3-D ADVECTION
!           1)  BY USING THE SCHEME     PROPOSED BY R.C.Yu
!           2)  BY USING THE SCHEME     PROPOSED BY P.K.Smolarkiewicz
!
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
# 112 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/QPDATA.F" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/PARADD" 1
      INTEGER IB,IE,JB,JE,KE,NM
      PARAMETER ( IB=2,IE=NX-1,JB=2,JE=NY-1,KE=NL+2,NM=NL-1 )
# 113 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/QPDATA.F" 2

C
      REAL*8 U0(NX,NY,NL),V0(NX,NY,NL),W0(NX,NY,NL)
      REAL*8 U(NX,NY,NL),V(NX,NY,NL),WS(NX,NY,NZ)
      REAL*8 QT(NX,NY,NL),DQ(NX,NY,NL)
      REAL*8 DSGHL(NL)
!
      INTEGER NONOS,IORD,ISOR,IP(NX)
!     REAL*8 EP,DSNP,DSSP,DTDLN(NY),DTDLT(NY),GC(NY),DTDSG(NL)
!
      REAL*8 DSNP,DSSP,DTDLN(NY),DTDLT(NY),GC(NY),DTDSG(NL)
      REAL*8 UQ(NX,NY,NL),VQ(NX,NY,NL),WQ(NX,NY,NL),PQ(NX,NY)
      REAL*8 HALF,ONE
      DATA    HALF,ONE / 0.5E0,1.0E0 /
      INTEGER I,J,K
      integer begj,endj


      begj = 2
      endj = ny-1





!
!
!     GET THE ADVECTION VELOCITY
!
      DO K   = 1 ,NL
         DO J   = begj , endj
            DO I   = 1 ,NX
               UQ(I,J,K)  = HALF*(U (I,J,K)+U0(I,J,K))
               VQ(I,J,K)  = HALF*(V (I,J,K)+V0(I,J,K))
               WQ(I,J,K)  = HALF*(WS(I,J,K)+W0(I,J,K))
            ENDDO
         ENDDO
!
!     SAVE THE FIELD ON INPUT
!
         DO J   = begj , endj
            DO I   = 1 ,NX
               DQ(I,J,K)    = QT(I,J,K)
            ENDDO
         ENDDO
      ENDDO
!
!     PERFORM HORIZONTAL ADVECTION IN SPHERICAL GEOMETRY
!
      DO J = begj , endj
         DO I = 1 ,NX
            PQ(I,J)    = ONE
         ENDDO
      ENDDO
!
!     DO THE 2-D ADVECTION BY MPDATA
!
      CALL TSPAS(QT,UQ,VQ,PQ,DSNP,DSSP,GC,DTDLT,DTDLN)
!
!     PERFORM THE VERTICAL ADVECTION
!       BY  R.C.Yu
      CALL TSPASW(QT,WQ,DTDSG)
!
!     PERFORM VERTICAL REDISTRIBUTION TO AVOID NEGATIVE Q-H2O
!
      CALL AVNEGQ(QT,DSGHL)
      RETURN
      END
