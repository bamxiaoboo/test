# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/VPAR.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/VPAR.F"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/VPAR.F" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/VPAR.F" 2

! (wanhui 2003.04.03)
! -------------------
	subroutine vpar (  pmtop, p0, sig, sigl, dsig )

        implicit none


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
# 11 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/VPAR.F" 2

        real*8  p0          ! model bottom pressure,     intent( in)
        real*8  pmtop       ! model top pressure,        intent(out)
        real*8  sig (nz)    ! sigma at model interfaces, intent(out)
        real*8  sigl(nl)    ! sigma at model levels,     intent(out)
	real*8  dsig(nl)    ! vertical stepsize,         intent(out)

	real*8  ps0         ! referrence surface pressre of cam2
        real*8  hyai(nz)    ! parameter 'a' in cam2 pressure formula
	real*8  hybi(nz)    ! parameter 'b' in cam2 pressure formula
	integer k           ! layer index

        data ps0  /1.0d3/   ! unit: hPa

        data hyai /0.00219406700000001d0, 0.00489520900000001d0,
     _             0.009882418d0,         0.01805201d0,
     _             0.02983724d0,          0.0446233400000002d0,
     _             0.0616058700000002d0,  0.0785124300000004d0,
     _             0.0773127100000002d0,  0.0759013100000003d0,
     _             0.0742408600000002d0,  0.0722874400000002d0,
     _             0.0699893299999998d0,  0.06728574d0,
     _             0.06410509d0,          0.0603632200000002d0,
     _             0.0559611100000001d0,  0.0507822500000001d0,
     _             0.0446896000000001d0,  0.0375219099999999d0,
     _             0.0290894900000001d0,  0.02084739d0,
     _             0.01334443d0,          0.00708499000000001d0,
     _             0.00252136d0,          0.0d0,
     _             0.0d0/

        data hybi /0.0d0,                 0.0d0,
     _             0.0d0,                 0.0d0,
     _             0.0d0,                 0.0d0,
     _             0.0d0,                 0.0d0,
     _             0.01505309d0,          0.03276228d0,
     _             0.05359622d0,          0.0781062700000006d0,
     _             0.1069411d0,           0.140863700000001d0,
     _             0.180772d0,            0.227722d0,
     _             0.282956200000001d0,   0.347936400000002d0,
     _             0.4243822d0,           0.514316800000003d0,
     _             0.620120200000002d0,   0.723535500000004d0,
     _             0.817676800000001d0,   0.896215300000001d0,
     _             0.953476100000003d0,   0.9851122d0,
     _             1.0d0/

!--------------------------------------------------------------

	pmtop = hyai(1)*ps0

        sig(1)= 0.0d0
        do k=1,nl
           sig (k+1)= (hyai(k+1)*ps0+hybi(k+1)*p0-pmtop)/(p0-pmtop)
	   sigl(k  )= 0.5d0*( sig(k)+sig(k+1) )
           dsig(k  )= sig(k+1)-sig(k)
        enddo
!	write(*,*) 'pmtop:',pmtop
!	write(*,'(1x,i3,3f20.16)')  (k,sig(k),sigl(k),dsig(k),k=1,nl)
!	write(*,'(1x,i3,f20.16)')   nz,sig(k)

	return
        end

