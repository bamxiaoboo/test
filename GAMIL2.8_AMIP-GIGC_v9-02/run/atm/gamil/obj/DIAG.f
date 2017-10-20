# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIAG.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIAG.F"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIAG.F" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIAG.F" 2

!!(2003.11.29)
!!-------------------

	SUBROUTINE DIAG(U,V,P,PS,PLY,TT,US,VS,TS,H,HPS
     _               ,PMTOP,PSB,TBS,TB,CB,FAC,DSIG)
!
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
# 13 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIAG.F" 2



# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/commpi.h" 1

        
      integer nprocs, myrank, ierr, itop, ibot, jbeg, jend, jpole
      logical inc_pole

      common/commpi/ nprocs, myrank, itop, ibot, jbeg, jend, jpole, inc_pole
# 16 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/DIAG.F" 2
      character*50 filename

!
!	The file PARA is to define the parameters related to the model resolution:
!     NX is the grid number in longitude
!     NY is the grid number in latitude
!     NL is the number of vertical layers
!
	REAL*8
     _     U  (NX,NY,NL)  !  U=u*sqrt(Ps), input variable
     _    ,V  (NX,NY,NL)  !	 V=v*sqrt(Ps), input variable
     _    ,P  (NX,NY   )  !  P=sqrt(Ps)  , input variable
     _    ,PS (NX,NY   )  !	 Surface pressure, input variable
     _    ,PLY(NX,NY,NL)  !  PLY=p, Pressure in Sigma Layer, input variable
     _    ,TS (NX,NY,NL)  !  input variable, TS=T, TEMPERATURE
     _    ,TT (NX,NY,NL)  !  TT=R*T'*Sqrt(Ps)/CB, input variable
!                            where T'=T-TB, T is the temperatur,
!                            TBB is Temperature of the standard atmosphere
     _    ,TB (NX,NY,NL)  !  input variable,
     _    ,CB (NX,NY,NL)  !  CB=Sqrt(R*(KK*TBB-dTB/dlnp)), input variable,
!                            where, KK=R/Cp, R is a constant
     _    ,PMTOP          !  PMTOP=10hPa
     _    ,TBS(NX,NY   )  !  TBB at the surface, input constant
     _    ,PSB(NX,NY   )  !  PSB is the surface pressure of the standard
!					 atmosphere, input constant
     _    ,DSIG(NL     )  !  The vertical stepsizes, input constant
     _	  ,US(NX,NY,NL )  !  US = u, zonal wind,    output variable
     _    ,VS(NX,NY,NL )  !  VS = v, meridional wind,output variable
     _    ,HPS(NX,NY   )  !  the surface geopotential height deviation
     _	  ,H (NX,NY,NZ )  !  H=gz-HBB, gz is the geopotential height,
!                              HBB is the geopotential height of the standard atmopshere
     _    ,FAC(NX,NY,NZ)
     _    ,WK1,WK2        !  working variables
!
	INTEGER I,J,K
        integer begj,endj


      begj = 2
      endj = ny-1





      DO J=begj,endj
        DO K=1,NL
          DO I=2,NX-1
            WK1=2.0D0/(P(I,J)+P(I-1,J))

            if ( (myrank.eq.0).and.(j.eq.endj) ) then
              WK2=0.0D0
            else
              WK2=2.0D0/(P(I,J)+P(I,J+1))
            endif







            US(I,J,K)=WK1*U(I,J,K)
            VS(I,J,K)=WK2*V(I,J,K)
	    TS(I,J,K)=TT(I,J,K)*CB(I,J,K)/(P(I,J)*RD)+TB(I,J,K)
          ENDDO
	  US(1,J,K)=US(NX-1,J,K)
	  US(NX,J,K)=US(2,J,K)
	  VS(1,J,K)=VS(NX-1,J,K)
	  VS(NX,J,K)=VS(2,J,K)
	  TS(1,J,K)=TS(NX-1,J,K)
	  TS(NX,J,K)=TS(2,J,K)
        ENDDO
C
C     CALCULATING H(K=NZ)=R*TB(PSB)/PSB*(PS-PSB) AS WELL AS H(K.LT.NZ).
        DO I=2,NX-1
	  HPS(I,J   )=RD*TBS(I,J)/PSB(I,J)*(PS(I,J)+PMTOP-PSB(I,J))
          H  (I,J,NZ)=HPS(I,J)
        ENDDO
	HPS(1,J   ) =HPS(NX-1,J)
	HPS(NX,J   )=HPS(2,J)
	H  (1,J,NZ) =H(NX-1,J,NZ)
	H  (NX,J,NZ)=H(2,J,NZ)
        DO K=NL,1,-1
          DO I=1,NX
           H(I,J,K)=H(I,J,K+1)+DSIG(K)*P(I,J)*CB(I,J,K)
     &       /PLY(I,J,K)*TT(I,J,K)*.5*(FAC(I,J,K+1)+FAC(I,J,K))
          ENDDO
	  H(1,J,K)=H(NX-1,J,K)
	  H(NX,J,K)=H(2,J,K)
        ENDDO
      ENDDO
c
!- check ---------------------------------------------------------
!
!#if (defined )
!      write(filename,14) 'diag-p-',myrank,'.out'
!14    format(a7,i1,a4)
!      open (10,file=trim(filename))
!#else
!      open (10,file='diag-s.out')
!#endif
!
!      write(10,*) '------------------- us -----------------'
!      write(10,11) (j,(us(i,j,10),i=1,2),j=1,ny)
!
!      write(10,*) '------------------- vs -----------------'
!      write(10,11) (j,(vs(i,j,10),i=1,2),j=1,ny)
!
!      write(10,*) '------------------- ts -----------------'
!      write(10,11) (j,(ts(i,j,10),i=1,2),j=1,ny)
!
!      write(10,*) '------------------- h -----------------'
!      write(10,11) (j,(h(i,j,10),i=1,2),j=1,ny)
!
!11    format(1x,i5,2e30.20)
!
!
!      close (10)
!
!#if (defined )
!!      call mpi_finalize(j)
!#endif
!!      stop 'diag'
!
!-----------------------------------------------------------------

	RETURN
	END
