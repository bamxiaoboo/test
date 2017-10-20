# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/SPAN.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/SPAN.F"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/SPAN.F" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/SPAN.F" 2



!!(wh 2003.10.27)
!!-------------------

      SUBROUTINE SPAN(MM1,MM2,MM3,MP1,MP2,MP3,MDJ)

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
# 14 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/SPAN.F" 2


# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/commpi.h" 1

        
      integer nprocs, myrank, ierr, itop, ibot, jbeg, jend, jpole
      logical inc_pole

      common/commpi/ nprocs, myrank, itop, ibot, jbeg, jend, jpole, inc_pole
# 16 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/SPAN.F" 2

      INTEGER MM1(NX,NY),MM2(NX,NY),MM3(NX,NY)
      integer MP1(NX,NY),MP2(NX,NY),MP3(NX,NY),MDJ(NY)

      INTEGER I,J
      integer index   ! global index of each latitude
      integer max,min

      character*50 filename


!************************************************************************
!               Set the values to MM1,MP1;MM2,MP2;MM3,MP3               *
!                    MM1=i-mdj,if MM1<2, MM1=NX-2+MM1                   *
!                  MP1=i+mdj,if MP1>NX-1, MP1=MP1-NX+2                  *
!                 MM2=i-(mdj-1)/2,if MM2<2, MM2=NX-2+MM2                *
!               MP2=i+(mdj+1)/2,if MP2>NX-1, MP2=MP2-NX+2               *
!                 MM3=i-(mdj+1)/2,if MM3<2, MM3=NX-2+MM3                *
!               MP3=i+(mdj-1)/2,if MP3>NX-1, MP3=MP3-NX+2               *
!************************************************************************
!!      MDJ(1)=3
!!      MDJ(2)=3
!!      MDJ(3)=3
!!      MDJ(4)=3
!!      MDJ(NY)=3
!!      MDJ(NY-1)=3
!!      MDJ(NY-2)=3
!!      MDJ(NY-3)=3
!!      DO J=5,NY-4
!!         MDJ(J)=1
!!      END DO

       min = 4
       max = nprocs*(ny-2)-3

       do j=2,ny-1
          index = (nprocs-1-myrank)*(ny-2)+(j-1)
          if ( (index.le.min).or.(index.ge.max) ) then
!              mdj(j) = 3
               mdj(j) = 1
          else
	       mdj(j) = 1
          endif
       enddo

!!    DO J=1,NY
      do j=2,ny-1
      DO I=1,NX
	 MM1(I,J)=I-MDJ(J)
	 MP1(I,J)=I+MDJ(J)
	 MM2(I,J)=I-(MDJ(J)-1)/2
	 MP2(I,J)=I+(MDJ(J)+1)/2
	 MM3(I,J)=I-(MDJ(J)+1)/2
	 MP3(I,J)=I+(MDJ(J)-1)/2
	 IF (MM1(I,J).LT.2) MM1(I,J)=MM1(I,J)+NX-2
	 IF (MP1(I,J).GE.NX) MP1(I,J)=MP1(I,J)-NX+2
	 IF (MM2(I,J).LT.2) MM2(I,J)=MM2(I,J)+NX-2
	 IF (MP2(I,J).GE.NX) MP2(I,J)=MP2(I,J)-NX+2
	 IF (MM3(I,J).LT.2) MM3(I,J)=MM3(I,J)+NX-2
	 IF (MP3(I,J).GE.NX) MP3(I,J)=MP3(I,J)-NX+2
      END DO
      END DO
!******************           Made by Wang Bin           *****************
!*************************************************************************

!- check ----------------------------------------------------------------
!      write(filename,12) 'span-p-',myrank,'.out'
!12    format(a7,i1,a4)
!
!      open (10,file=trim(filename))
!      write(10,11) mdj
!      write(10,*) '-------------------mp2-----------------'
!      do j=1,ny
!        write(10,*) 'j=',j,'----------------------'
!        write(10,11) (mp2(i,j),i=1,nx)
!11    format(1x,10i9)
!      enddo
!      close (10)
!
!      call mpi_finalize(j)
!      stop 'span'
!----------------------------------------------------------------(wh)----


	RETURN
	END


