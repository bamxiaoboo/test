# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/initcom.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/initcom.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/initcom.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/initcom.F90" 2

subroutine initcom

!! (wanhui 2003.04.30)
!! (wanhui 2003.07.10)
!! (wanhui 2004.04.14)
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Initialize Model commons, including COMCON, COMHYB, COMMAP, COMSPE,
! and COMTRCNM
! 
! Method: 
! 
! Author: 
! Original version:  CCM1
! Standardized:      L. Bath, Jun 1992
!                    L. Buja, Feb 1996
!
!-----------------------------------------------------------------------
!
! $Id: initcom.F90,v 1.16.2.2 2002/06/15 13:47:48 erik Exp $
! $Author: erik $
!
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8
   use pmgrid
!! use pspect
!! use comspe
   use rgrid
!! use gauaw_mod, only: gauaw
   use commap
!! use prognostics, only: phis
   use comfm1, only: ghs
!! use dynconst, only: rearth, ra, dynconsti
!! use physconst, only: rair
   use constituents, only: ppcnst, qmin, qmincg               !!(wh 2003.10.24)
!! use constituents, only: ppcnst, qmin, qmincg, &
!!                           isor, iord, ipq, dsnp,dssp, gc, dtdlt,dtdln,dtdsg
   use qadv, only:  isor, iord, ipq, dsnp,dssp, gc, dtdlt,dtdln,dtdsg,initialize_qadv !!(wh)
!! use time_manager, only: get_step_size
   use time_manager, only: get_step_size,dtdy  !! (wh 2004.04.14)
   use stdatm                           !!(wh 2003.10.23)
   use comhd                            !!(wh 2003.10.23)
   use fspan                            !!(wh 2003.10.24)
!-----------------------------------------------------------------------
   implicit none
!-----------------------------------------------------------------------

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/comctl.h" 1
!----------------------------------------------------------------------- 
! 
! Purpose: Model control variables
! 
! Author: CCM Core Group
! 
!-----------------------------------------------------------------------
!! (wh 2003.04.30)
!! (wh 2003.12.27)

      common /comctl/ itsst   ,nsrest  ,iradsw  ,iradlw  ,iradae
      common /comctl/ nrefrq
      common /comctl/ anncyc  ,nlend   ,nlres   ,nlhst   ,lbrnch
      common /comctl/ aeres   ,ozncyc  ,sstcyc  ,icecyc
      common /comctl/ adiabatic,flxave
      common /comctl/ trace_gas, trace_test1,trace_test2, trace_test3
!!    common /comctl/ readtrace,ideal_phys, nsplit, iord, jord, kord, use_eta, aqua_planet
      common /comctl/ readtrace,ideal_phys,                                    aqua_planet
      common /comctl/ doRamp_ghg, doRamp_so4, doRamp_scon, fullgrid, doIPCC_so4, &
                      doCmip5_scon,doCmip5_ghg  !!(wh)
      common /comctl/ print_step_cost
      common /comctl/ doabsems, dosw, dolw, indirect

!!    common /comctl_r8/ divdampn, precc_thresh, precl_thresh
      common /comctl_r8/           precc_thresh, precl_thresh

      integer itsst             ! Sea surf. temp. update freq. (iters)
      integer nsrest            ! Restart flag
      integer iradsw            ! Iteration freq. for shortwave radiation
      integer iradlw            ! Iteration freq. for longwave radiation
      integer iradae            ! Iteration freq. for absorptivity/emissivity
      integer nrefrq            ! Restart write freq.

! f-v dynamics specific
! _ord = 1: first order upwind
! _ord = 2: 2nd order van Leer (Lin et al 1994)
! _ord = 3: standard PPM 
! _ord = 4: enhanced PPM (default)
!!      integer nsplit            ! Lagrangian time splits (Lin-Rood only)
!!      integer iord              ! scheme to be used in E-W direction
!!      integer jord              ! scheme to be used in N-S direction
!!      integer kord              ! scheme to be used for vertical mapping
!!      logical use_eta           ! Flag to use a's and b's set by dynamics/lr/set_eta.F90

      logical aqua_planet       ! Flag to run model in "aqua planet" mode

      logical anncyc            ! true => do annual cycle (otherwise perpetual)
      logical nlend             ! true => end of run
      logical nlres             ! true => continuation run
      logical nlhst             ! true => regeneration run
      logical lbrnch            ! true => branch run
      logical aeres             ! true => read/write a/e data to/from restart file
      logical ozncyc            ! true => cycle ozone dataset
      logical sstcyc            ! true => cycle sst dataset
      logical icecyc            ! true => cycle ice fraction dataset
      logical adiabatic         ! true => no physics
      logical ideal_phys        ! true => run "idealized" model configuration
      logical flxave            ! true => send to coupler only on radiation time steps

      logical trace_gas         ! true => turn on greenhouse gas code
      logical trace_test1       ! true => turn on test tracer code with 1 tracer
      logical trace_test2       ! true => turn on test tracer code with 2 tracers
      logical trace_test3       ! true => turn on test tracer code with 3 tracers
      logical readtrace         ! true => obtain initial tracer data from IC file

      logical doRamp_ghg        ! true => turn on ramping for ghg
      logical doRamp_so4        ! true => turn on ramping for so4
      logical doRamp_scon       ! true => turn on ramping for scon
      logical doIPCC_so4        ! true => turn on IPCC scenario for so4  !!(wh) 
      logical doCmip5_scon      !
      logical doCmip5_ghg       ! ljli2010-08-12
      logical fullgrid          ! true => no grid reduction towards poles

      logical print_step_cost   ! true => print per-timestep cost info

      logical doabsems          ! True => abs/emiss calculation this timestep
      logical dosw              ! True => shortwave calculation this timestep
      logical dolw              ! True => longwave calculation this timestep
      logical indirect          ! True => include indirect radiative effects of sulfate aerosols

!!    real(r8) divdampn         ! Number of days to invoke divergence damper
      real(r8) precc_thresh     ! Precipitation threshold for PRECCINT and PRECCFRQ
      real(r8) precl_thresh     ! Precipitation threshold for PRECLINT and PRECLFRQ
# 52 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/initcom.F90" 2
!-----------------------------------------------------------------------
!!#include <comfft.h>
!-----------------------------------------------------------------------

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/comhyb.h" 1
!----------------------------------------------------------------------- 
! 
! Purpose: Hybrid level definitions: p = a*p0 + b*ps
!          interfaces   p(k) = hyai(k)*ps0 + hybi(k)*ps
!          midpoints    p(k) = hyam(k)*ps0 + hybm(k)*ps
! 
!-----------------------------------------------------------------------
!!!  vertical level definitions in LASG dynamical core: p = pes*sigma + pt
!!!        interfaces   ply(k) = ps*sig (k) + pmtop
!!!        midpoints    ply(k) = ps*sigl(k) + pmtop
!!!---------------------------------------------------------------------
!!!(wanhui 2003.04.30)
!!!(wanhui 2003.10.23)  (std.atm. variables removed)

      real(r8) hyai(plevp)       ! ps0 component of hybrid coordinate - interfaces
      real(r8) hybi(plevp)       ! ps component of hybrid coordinate - interfaces
      real(r8) hyam(plev)        ! ps0 component of hybrid coordinate - midpoints
      real(r8) hybm(plev)        ! ps component of hybrid coordinate - midpoints

!!    real(r8) hybd(plev)        ! difference  in b (hybi) across layers
      real(r8) hypi(plevp)       ! reference pressures at interfaces
      real(r8) hypm(plev)        ! reference pressures at midpoints
!!    real(r8) hypd(plev)        ! reference pressure layer thickness

      real(r8) ps0         ! base state sfc pressure for level definitions
!!    real(r8) psr         ! reference surface pressure for linearization
!!    real(r8) prsfac      ! log pressure extrapolation factor (time, space independent)

!!    integer nprlev       ! number of pure pressure levels at top

      real(r8) :: pmtop              !
      real(r8) :: sig (plevp)        !
      real(r8) :: sigl(plev)         !  fm2003 VPAR variables
      real(r8) :: dsig(plev)         !

!!(wanhui 2003.10.23)
!!------------------------------------------------------------
!!    real(r8) :: tbb (plevstd)         !
!!    real(r8) :: hbb (plevstd)         !
!!    real(r8) :: cbb (plevstd)         !
!!    real(r8) :: dcbb(plevstd)         !  fm2003 std. atm.
!!    real(r8) :: p00, t00              !
!!    real(r8) :: psb (plond,plat)      !
!!    real(r8) :: tsb (plond,plat)      !
!!------------------------------------------------------------
!!(2003.10.23)(these variables are in module stdatm now)


!!      common /comhyb/ hyai ,hyam  ,hybi ,hybm
!!      common /comhyb/ hybd ,hypi ,hypm  ,hypd
!!      common /comhyb/ ps0         ,psr         ,prsfac      ,nprlev

      common /comhyb/ hyai ,hybi ,hyam ,hybm
      common /comhyb/ hypi ,hypm
      common /comhyb/ ps0  ,pmtop
      common /comhyb/ sig  ,sigl , dsig
!!    common /comhyb/ tbb  ,hbb  , cbb  ,dcbb  ,p00 ,t00 ,psb ,tsb    !!(wh 2003.10.23)
 
# 56 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/initcom.F90" 2
!-----------------------------------------------------------------------
!!#include <comhd.h>
!!----------------------------------------------------------------------
! Local workspace
!

   integer i           ! longitude index
   integer j           ! Latitude index
   integer begj

   integer m           ! lengendre array index

   logical lprint      ! Debug print flag
   integer lat         ! Latitude index

   real(r8) pi             ! Mathematical pi (3.14...)
   real(r8) north,south

   real(r8) dtime          ! timestep size [seconds]
!
!-----------------------------------------------------------------------
     pi = 4.0*atan(1.0)

     begj=beglatexdyn

!!
!! Set the fm2003 std.atm.
!!
     call initialize_stdatm                 !!(wh 2003.10.23)
     call stdatm0(TBB,CBB,DCBB,HBB,P00,T00 )
     call setmsa0(TBB,HBB,ghs ,P00,T00,PSB,TSB)

!
     lprint = masterproc .and. .FALSE.

     dtime = get_step_size()
!!
!! Set vertical layers
!!
     call vpar (pmtop,p00,sig,sigl,dsig )

     pmtop = pmtop * 100.0d0
     p00   = p00   * 100.0d0

!!
!! calculate hypi & hypm for 'inti'
!!
     call hycoef
    
!!
!! Initialize commap.
!!
     call initialize_fspan                   !!(wh 2003.10.24)
     call span (mm1,mm2,mm3,mp1,mp2,mp3,mdj)

!!     
     call latmesh (dy,ythu(1),ythv(1),wtgu(1),wtgv(1))


      w(1)    = 1-cos(0.5*ythu(2))
      w(plat) = w(1) 

      do j=2,plat/2
         north = 0.5*( ythu(j-1)+ythu(j) )
         south = 0.5*( ythu(j+1)+ythu(j) )
         w(j) = cos(north)-cos(south)
         w(plat+1-j) = w(j)
      enddo

      do j=1,plat
         clat(j) = ythu(j)-0.5d0*pi     
      enddo
 
      do lat=1,plat
         latdeg(lat) = clat(lat)*45./atan(1._r8)
      end do
!!
     dx = pi*2.0/dble(plon)

     call initialize_hpar                   !!(wh 2003.10.24)
     call hpar (dx,dy,ythu(begj),ythv(begj),wtgu(begj),wtgv(begj),mdj            &
                     ,sinu,sinv,oux,ouy,ovx,ovy,ff,cur)
!!
!!
!! Set parameters for horizontal diffusion
!!
     call initialize_comhd             !!(wh 2003.10.23)

!!   dfs0 = 0.02d0                     !!(namelist variable.  wh 2004.04.14)
     dthdfs = dtime 
     
     call stdfsc (dfs0,dthdfs,sinu,sinv,wtgu(begj),wtgv(begj),dx,dy   &
                   ,frdt,frds,frdu,frdv,frdp,dxvpn,dxvps)

!!
!! Set parameters for the advection integration of q-H2O
!!
     call initialize_qadv               !!(wh 2003.10.24)

     call conpda (dtdy,dx,dy,sinu,wtgv(begj),dsig,-1.0d0,isor,iord   &
                          ,ipq,dsnp,dssp,gc,dtdlt,dtdln,dtdsg) 

!
! Set minimum mixing ratio for moisture and advected tracers
!
   qmin(1) = 1.e-12          ! Minimum mixing ratio for moisture
   do m=2,ppcnst
      qmin(m) = 0.0
   end do
!
! Set the minimum mixing ratio for the counter-gradient term.  
! Normally this should be the same as qmin, but in order to 
! match control case 414 use zero for water vapor.
!
   qmincg(1) = 0.
   do m=2,ppcnst
      qmincg(m) = qmin(m)
   end do
!
!
! Determine whether full or reduced grid
!
   fullgrid = .true.
   do j=1,plat
      if (masterproc) then
         write(6,*)'nlon(',j,')=',nlon(j)
      end if
      if (nlon(j).lt.plon) fullgrid = .false.
   end do
!
!
! Longitude array
!
   do lat=1,plat
      do i=1,nlon(lat)
         londeg(i,lat) = (i-1)*360./nlon(lat)
         clon(i,lat)   = (i-1)*2.0*pi/nlon(lat)
      end do
   end do

!
! Set flag indicating dynamics grid is now defined.
! NOTE: this ASSUMES initcom is called after spmdinit.  The setting of nlon done here completes
! the definition of the dynamics grid.
!
   dyngrid_set = .true.

   return

9910 format( 1x,i3,13f9.5)
9920 format(/,      13i9)

end subroutine initcom


