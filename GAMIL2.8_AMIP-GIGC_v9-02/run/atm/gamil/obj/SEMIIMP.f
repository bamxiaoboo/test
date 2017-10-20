# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/SEMIIMP.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/SEMIIMP.F"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/SEMIIMP.F" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/SEMIIMP.F" 2
!
        subroutine semiu(uu,hh,p,du,dps
     _                  ,up,psp,dt,oux,dsig,c)
c
        implicit none


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
# 10 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/SEMIIMP.F" 2



# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/commpi.h" 1

        
      integer nprocs, myrank, ierr, itop, ibot, jbeg, jend, jpole
      logical inc_pole

      common/commpi/ nprocs, myrank, itop, ibot, jbeg, jend, jpole, inc_pole
# 13 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/SEMIIMP.F" 2



        integer i,j,i1,i2,i0,k,np
	parameter(np=nx-1)
        real*8
     &   uu(nx,ny),hh(nx,ny),p(nx,ny),up(nx,ny),psp(nx,ny)
     &  ,du(nx,ny,nl),dps(nx,ny),dsig(nl)
     &  ,dt,oux(ny),c(nx,ny)
        real*8 ai(nx),bi(nx),di(nx)
	real*8 dx,us(nx),dus(nx)
	real*8 al(np-2,5),ak(np),wh(nx),byy,xx
c
c       Semi-implicit scheme
c
c

        do j=jbeg,jend



           dx=0.25d0*oux(j)*dt
	     do i=2,np
	        dus(i)=0.0d0
	        do k=1,nl
		   dus(i)=dus(i)+du(i,j,k)*dsig(k)
		end do
	     enddo
!
           do i=2,np
              i1=i-1
              i2=i+1
              if (i1.eq.1) i1=np
              if (i2.eq.nx) i2=2
	        ai(i)=dx*c(i,j)*(p(i2,j)+p(i,j))
		bi(i)=dx*c(i,j)*(p(i,j)+p(i1,j))
		di(i)=dx*c(i1,j)*(p(i,j)+p(i1,j))
!
	        us(i)=uu(i,j)+dt*dus(i)
     _			 -bi(i)*hh(i,j)+di(i)*hh(i1,j)
		wh(i)=hh(i,j)+dt*dps(i,j)*c(i,j)
     _                   -ai(i)*uu(i2,j)+bi(i)*uu(i,j)
	     end do
c
           do i=2,np
              i1=i-1
              i2=i+1
              if (i1.eq.1) i1=np
              if (i2.eq.nx) i2=2
              if (i.lt.np) then
                 al(i-1,1)=-bi(i1)*di(i)
                 al(i-1,2)=1.0d0+bi(i)*bi(i)+ai(i1)*di(i)
                 al(i-1,3)=-ai(i)*bi(i)
                 al(i-1,4)=0.0
                 al(i-1,5)=us(i)-bi(i)*wh(i)+di(i)*wh(i1)
              else
                do i0=2,np
                 if (i0.eq.i ) then
                    ak(i0-1)= 1.0d0+bi(i)*bi(i)+ai(i1)*di(i)
                 else if (i0.eq.i1) then
                    ak(i0-1)=-bi(i1)*di(i)
                 else if (i0.eq.i2) then
                    ak(i0-1)=-ai(i)*bi(i)
                 else
                    ak(i0-1)=0.0d0
                 end if
                end do
                ak(np)=us(i)-bi(i)*wh(i)+di(i)*wh(i1)
              end if
           end do
!
           al(1,4)=al(1,1)
           al(1,1)=0.0
           al(np-2,4)=al(np-2,3)
           al(np-2,3)=0.0
           call gauss(al,ak,np-1)
           do i=2,np-1
              up(i,j)=al(i-1,5)
           end do
           i=np
           up(i,j)=ak(np)
	   up(1,j)=ak(np)
	   up(nx,j)=up(2,j)
c
	   do i=2,np
              i1=i-1
              i2=i+1
              if (i1.eq.1) i1=np
              if (i2.eq.nx) i2=2
		    psp(i,j)=wh(i)-ai(i)*up(i2,j)+bi(i)*up(i,j)
		    psp(i,j)=(psp(i,j)-hh(i,j))/(dt*c(i,j))-dps(i,j)
	   end do
	   psp(1,j)=psp(np,j)
	   psp(nx,j)=psp(2,j)
c
	   do i=2,np
              i1=i-1
              i2=i+1
              if (i1.eq.1) i1=np
              if (i2.eq.nx) i2=2
		    up(i,j)=(up(i,j)-uu(i,j))/dt-dus(i)
	   end do
	   up(1,j)=up(np,j)
	   up(nx,j)=up(2,j)
        end do
c

      if ( myrank.eq.(nprocs-1) ) then



        do i=1,nx
           up(i,jpole)=0.0
           psp(i,jpole)=0.0
        end do

      endif

!

      if ( myrank.eq.0 ) then



        do i=1,nx
           up(i,jpole)=0.0
           psp(i,jpole)=0.0
        end do

      endif

c
c       End of semi-implicit quadratic-conservation scheme
c
        return
        end
