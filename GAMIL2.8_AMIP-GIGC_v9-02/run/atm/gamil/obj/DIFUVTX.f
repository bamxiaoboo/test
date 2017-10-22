# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIFUVTX.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIFUVTX.F"
      SUBROUTINE DIFUVTX(US,US1,U,V,TT,OUX,OVX
     _                 ,MP1,MP2,MM1,MM2,DU,DV,DTT,J)
!
      IMPLICIT NONE
!
!	This subroutine is to calculate the tendency of the wind and the temperature:
!     DU, DV, DTT
!

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/PARADYN" 1

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/PARADYN" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/PARADYN" 2

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
# 10 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIFUVTX.F" 2
!
!	The file PARA is to define the parameters related to the model resolution:
!     NX is the grid number in longitude
!     NY is the grid number in latitude
!     NL is the number of vertical layers
!
	  REAL*8
     _       U  (NX,NL)  !  input variable, U  = u*sqrt(Ps)
     _	    ,US (NX,NL)  !  input variable, US = u, zonal wind
     _	    ,US1(NX,NL)  !  input variable, US = u, zonal wind
     _      ,V  (NX,NL)  !  input variable, V  = v*sqrt(Ps)
     _      ,TT (NX,NL)  !  input variable, TT=R*T'*Sqrt(Ps)/CB,
!                           where T'=T-TB, T is the temperatur,
!                           TBB	is Temperature of the standard atmosphere
     _      ,OUX         !  input constant, OUX=1/(RAD*SINU*DX*MDJ),where,
!                           DX is the horizontal stepsize in zonal direction,
!                           MDJ is the leaping length of the central difference
!                           SINU is sin(theta) at intger grid j
     _      ,OVX         !  input constant, OUX=1/(RAD*SINV*DX*MDJ)
!
	INTEGER
     _       MM1(NX)     !	 input constant
     _      ,MP1(NX)     !	 input constant
     _      ,MM2(NX)     !	 input constant
     _      ,MP2(NX)     !	 input constant
!
      REAL*8
     _       DU (NX,NL)	  !  output variables
     _      ,DV (NX,NL)   !  output variables
     _      ,DTT(NX,NL)  !  output variables
!
      INTEGER I,J,K
      INTEGER IM1,IP2,IP1,IM2,I1,J0,IM3,IP3
      REAL*8  OUX2,OUX4,OVX4
C
C     AS FOLLOWS, THE IMPROVED LEAP-FROG AND THE REGENERATION OF
C     VARIABLES WILL BE FINISHED IN THE SAME CYCLES.
C
      I1=NX-1
      OUX2=OUX*0.5
      OUX4=OUX*0.25
      OVX4=OVX*0.25
C
C     CALCULATING DU/DT, DV/DT AND DTT/DT.
C
      IF(J.EQ.1) THEN
	 DO K=1,NL
            DO I=2,I1
              IP1=MP1(I)
              IM1=MM1(I)
              IP2=MP2(I)
              IM2=MM2(I)
C
              DTT(I,K)=0.0D0
              DU (I,K)=0.0D0
              DV (I,K)=-OVX4*(US1(IP2,K)*V(IP1,K)
     &                       -US1(IM2,K)*V(IM1,K))
            ENDDO
         ENDDO
      ELSE
         DO K=1,NL
            DO I=2,I1
              IP1=MP1(I)
              IM1=MM1(I)
              IP2=MP2(I)
              IM2=MM2(I)
              DTT(I,K)=-OUX2*(US(IP2,K)*TT(IP1,K)-US(IM2,K)*TT(IM1,K))
              DU (I,K)=-OUX4*((US(I,K)+US(IP1,K))*U(IP1,K)
     &                         -(US(I,K)+US(IM1,K))*U(IM1,K))
              DV (I,K)=-OVX4*((US1(IP2,K)+US(IP2,K))*V(IP1,K)
     &                       -(US1(IM2,K)+US(IM2,K))*V(IM1,K))
            ENDDO
         ENDDO
      ENDIF
!
      DO K=1,NL
         DTT(1,K)=DTT(I1,K)
         DTT(NX,K)=DTT(2,K)
         DU (1,K)=DU (I1,K)
         DU (NX,K)=DU (2,K)
         DV (1,K)=DV (I1,K)
         DV (NX,K)=DV (2,K)
      ENDDO
!
      RETURN
      END
