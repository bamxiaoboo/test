# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIFUTX.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIFUTX.F"
      SUBROUTINE DIFUTX(U,P,TT,CB0,DSIG,OUX,DU,DTT)
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
# 10 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIFUTX.F" 2

!
!	The file PARA is to define the parameters related to the model resolution:
!     NX is the grid number in longitude
!     NY is the grid number in latitude
!     NL is the number of vertical layers
!
	  REAL*8
     _       U (NX,NL )  !  input variable, zonal wind velocity
     _      ,UU(NX,NL )  !
     _      ,UZ(NX,NZ )  !
     _      ,P  (NX   )  !  input variable, P  = sqrt(Ps)
     _      ,PP (NX   )  !  input variable, P  = sqrt(Ps)
     _      ,TT (NX,NL)  !  input variable, TT=R*T'*Sqrt(Ps)/CB,
!                              where T'=T-TB, T is the temperatur,
!                              TBB	is Temperature of the standard atmosphere
     _	  ,HZ(NX,NZ )  !  input variable,
     _	  ,HH(NX,NZ )  !  input variable,
!							 H=gz-HBB, gz is the geopotential height,
!                              HBB is the geopotential height of the standard atmopshere
     _      ,CB0(NX,NL)  !  input variable, CB=Sqrt(R*(KK*TBB-dTB/dlnp)),
!                           !  CB0=CB*P/PLY
!                              where, KK=R/Cp, R is a constant
     _      ,DSIG(NL  )  !  input constant, the vertical stepsizes
     _      ,OUX         !  input constant, OUX=1/(RAD*SINU*DX*MDJ)
!                              where, DX is the horizontal stepsize in zonal direction,
!                              MDJ is the leaping length of the central difference
!                              SINU is sin(theta) at intger grid j
      REAL*8
     _       DU(NX,NL)	  !  output variables
     _      ,DTT(NX,NL)  !  output variables
!
      INTEGER I,K,I1,K1
!
      I1=NX-1
!
!     CALCULATING THE INNER GRAVITY WAVE PART OF DU/DT AND DTT/DT.
!
	DO I=1,NX
	   UZ(I,1)=0.0
	   HZ(I,NZ)=0.0
	ENDDO
!
        P(1)=P(I1)
        P(NX)=P(2)
!
        DO I=2,I1
           PP(I)=0.5*(P(I)+P(I-1))
        END DO
!
	DO I=2,I1
	   DO K=1,NL
	      K1=K+1
	      UZ(I,K1)=UZ(I,K)+U(I,K)*DSIG(K)
	      UU(I,K)=0.5*(UZ(I,K)+UZ(I,K1))*PP(I)
	   ENDDO
	   DO K=NL,1,-1
	      K1=K+1
	      HZ(I,K)=HZ(I,K1)+TT(I,K)*CB0(I,K)*DSIG(K)
	      HH(I,K)=0.5*(HZ(I,K)+HZ(I,K1))
	   ENDDO
	ENDDO
!
	DO K=1,NL
	   UU(1,K)=UU(I1,K)
	   UU(NX,K)=UU(2,K)
	   HH(1,K)=HH(I1,K)
	   HH(NX,K)=HH(2,K)
	ENDDO
!
      DO K=1,NL
         DO I=2,I1
            DTT(I,K)=-CB0(I,K)*OUX*(UU(I+1,K)-UU(I,K))
            DU(I,K)=-OUX*PP(I)*(HH(I,K)-HH(I-1,K))
         ENDDO
         DTT(1,K)=DTT(I1,K)
         DTT(NX,K)=DTT(2,K)
         DU(1,K)=DU(I1,K)
         DU(NX,K)=DU(2,K)
      ENDDO
!
	RETURN
	END
