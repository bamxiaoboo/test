# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/readinitial.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/readinitial.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/readinitial.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/readinitial.F90" 2

! ***************************************************************************** !
! Purpose: 
!
!   Ensure that requisite netcdf variables are on the initial dataset.
!   Set base day and date info using the "current" values from it.
! 
! Method:
!
!   Issue proper netcdf wrapper calls.  Broadcast to slaves if 
! 
! Author:
!
!   CCM Core Group
!
! Development records:
!
!   1. old unclarified records
!      WAN Hui, 2003/04/30, 2003/07/10, 2003/10/20, 2003/10/27, 2003/11/22
!
! ***************************************************************************** !

subroutine readinitial(ncid)

    use shr_kind_mod, only: r8 => shr_kind_r8
    use pmgrid
    use rgrid
    use time_manager, only: ic_ymd, ic_tod

    use mpishorthand


    implicit none


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
# 38 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/readinitial.F90" 2

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
 
# 39 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/readinitial.F90" 2

    include 'netcdf.inc'

    integer, intent(in) :: ncid

    integer :: lonid            !------------------------------------------------------
    integer :: levid            ! 
    integer :: latid            ! 
    integer :: ncdateid         ! Netcdf variable and dimension ids for variable of that
    integer :: ncsecid          ! name with "id" tacked on to the end
    integer :: hyaiid           ! 
    integer :: hybiid           ! 
    integer :: hyamid           ! 
    integer :: hybmid           ! 
    integer :: ilev             ! 
    integer :: ilevid           ! 
    integer :: rlonid           ! 
    integer :: nlonid           ! 

    integer :: phisid
    real(r8):: phis_tmp(plond,plat)  !!(wh 2003.10.20)

    integer strt2d(3)           ! start lon, lat, time indices for netcdf 2-d !!(wh10.20)
    data strt2d/3*1/            ! only index 2 will ever change               !!(wh10.20)
    integer cnt2d(3)            ! lon, lat, time counts for netcdf 2-d        !!(wh10.20)
    data cnt2d/plon,1,1/        ! 2-d arrs: Always grab only a "plon" slice   !!(wh10.20)

    integer :: i, j

    integer :: mlon             ! longitude dimension length from dataset
    integer :: mlev             ! level dimension length from dataset
    integer :: morec            ! latitude dimension length from dataset






    if (masterproc) then
        !
        ! Get and check dimension/date info
        !
        call wrap_inq_dimid(ncid, 'lon' , lonid)
        call wrap_inq_dimid(ncid, 'lev' , levid)
        call wrap_inq_dimid(ncid, 'ilev', ilevid)
        call wrap_inq_dimid(ncid, 'lat' , latid)





        call wrap_inq_varid(ncid, 'date'   , ncdateid)
        call wrap_inq_varid(ncid, 'datesec', ncsecid)
        call wrap_inq_varid(ncid, 'hyai'   , hyaiid)
        call wrap_inq_varid(ncid, 'hybi'   , hybiid)
        call wrap_inq_varid(ncid, 'hyam'   , hyamid)
        call wrap_inq_varid(ncid, 'hybm'   , hybmid)
        call wrap_inq_varid(ncid, 'PHIS'   , phisid)

        call wrap_inq_dimlen(ncid, lonid , mlon)
        call wrap_inq_dimlen(ncid, levid , mlev)
        call wrap_inq_dimlen(ncid, ilevid, ilev)
        call wrap_inq_dimlen(ncid, latid , morec)
        !
        ! Check for reduced grid info on initial dataset.  If not present, define
        ! variables for full grid
        !
        if (nf_inq_varid(ncid, 'nlon', nlonid) == nf_noerr) then
            call wrap_get_var_int(ncid, nlonid, nlon)
        else
            nlon(:) = plon
        end if

        if (mlev /= plev .or. mlon /= plon .or. morec /= plat) then
            write(6, "('Error: readinitial: model parameters do not match initial dataset parameters')")
            write(6, "('  Model Parameters:   plev = ', I3, ' plon = ', I3, ' plat  = ', I3)") plev, plon, plat
            write(6, "('  Dataset Parameters: mlev = ', I3, ' mlon = ', I3, ' morec = ', I3)") mlev, mlon, morec
            call endrun
        end if

        call wrap_get_var_int(ncid, ncdateid, ic_ymd)
        call wrap_get_var_int(ncid, ncsecid , ic_tod)

        call wrap_get_var_realx(ncid, hyamid, hyam)
        call wrap_get_var_realx(ncid, hybmid, hybm)
        call wrap_get_var_realx(ncid, hyaiid, hyai)
        call wrap_get_var_realx(ncid, hybiid, hybi)

        !!(wh 2003.10.20)

        do j = 1, plat
            strt2d(2) = j
            call wrap_get_vara_realx(ncid, phisid, strt2d, cnt2d, phis_tmp(1,j))
        end do
        !!(wh)

        !! (wh 2003.10.20)
        !!      do j=1,plat
        !!        do i=1,plon
        !!            ghs(i,j) = phis_tmp(i,plat+1-j)
        !!        enddo
        !!      enddo
        !!
        !!      do j=1,plat
        !!         ghs(plond-1,j) = ghs(1,j)
        !!         ghs(plond,  j) = ghs(2,j)
        !!      enddo
        !! (wh)

    end if

    !!(wh 2003.10.20)

    call copy_phis_ghs(phis_tmp)


    call mpibcast (ic_ymd,  1,    mpiint, 0, mpicom)
    call mpibcast (ic_tod,  1,    mpiint, 0, mpicom)
    call mpibcast (nlon,    plat, mpiint, 0, mpicom)
    !! call mpibcast (wnummax, plat, mpiint, 0, mpicom)

    call mpibcast (hyam  ,plev ,mpir8,  0, mpicom)
    call mpibcast (hybm  ,plev ,mpir8,  0, mpicom)
    call mpibcast (hyai  ,plevp,mpir8,  0, mpicom)
    call mpibcast (hybi  ,plevp,mpir8,  0, mpicom)


    return
end subroutine readinitial

!!#############################
!!(wanhui 2003.10.20)
!!(wanhui 2003.10.27)

subroutine copy_phis_ghs( phis_tmp )

    use shr_kind_mod, only: r8 => shr_kind_r8
    use pmgrid
    use prognostics,  only: phis
    use comfm1,       only: ghs

    use mpishorthand
    use spmd_dyn,     only: npes, compute_gsfactors


    implicit none

    real(r8), intent(in) :: phis_tmp(plond,plat)

    integer i, jdyn, jcam, begj, endj

    character(50) filename


    integer numperlat         ! number of values per latitude band
    integer numsend(0:npes-1) ! number of items to be sent
    integer numrecv           ! number of items to be received
    integer displs(0:npes-1)  ! displacement array

    !-----------------------------------------------------------------
    !! scatter phis_tmp to phis
    !!----------------------------------------------------------------



    numperlat = plond
    call compute_gsfactors(numperlat, numrecv, numsend, displs)

    call mpiscatterv (phis_tmp, numsend, displs, mpir8, &
        phis(1,beglat), numrecv, mpir8, 0, mpicom)





    !!----------------------------------------------------------------
    !! copy phis to ghs
    !!----------------------------------------------------------------

    begj = beglatexdyn + numbnd
    endj = endlatexdyn - numbnd
    do jdyn = begj,endj
        jcam = beglatexdyn + endlatex - jdyn
        do i=1,plon
            ghs(i,jdyn) = phis(i,jcam )
        enddo
        ghs(plond-1,jdyn) = ghs(1,jdyn)
        ghs(plond,  jdyn) = ghs(2,jdyn)
    enddo

    !- check -------------------------------------------------------
    !
    !#if (defined )
    !   
    !       write(filename,12) 'ghs-p-',iam,'.out'
    !12     format(a6,i1,a4)
    !       open(10,file=trim(filename))
    !#else
    !       open(10,file='ghs-s.out')
    !#endif
    !
    !        write(10,*) '----------------- ghs -----------------'
    !        do jdyn=begj,endj
    !           write(10,11) jdyn,(ghs(i,jdyn),i=1,3)
    !        enddo
    !11      format(1x,i3,3e30.20)
    !       close(10)
    !!       stop
    !--------------------------------------------------------------

    return
end subroutine copy_phis_ghs
