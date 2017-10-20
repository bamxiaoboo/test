# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/inital.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/inital.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/inital.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/inital.F90" 2

! define initial conditions for first run of case

subroutine inital

    use shr_kind_mod, only: r8 => shr_kind_r8
    use pmgrid,       only: plon, plond, masterproc, &
        i1 ! added by ljli 2005/08/12
    use inidat,       only: read_inidat
    use prognostics,  only: ps, u3, v3, t3, q3, ptimelevels, initialize_prognostics, &
        q31, t31, q32, t32 ! added by LI Lijuan
    use buffer,       only: initialize_buffer
    use comsrf,       only: initialize_comsrf
    use ioFileMod,    only: getfil
    use radae,        only: initialize_radbuffer
    use phys_grid,    only: phys_grid_init
    use time_manager, only: timemgr_init
    use filenames,    only: ncdata
    use comfm1,       only: initialize_comfm1 ! WAN Hui 2003/10/23




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
# 29 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/inital.F90" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/comlun.h" 1
!----------------------------------------------------------------------- 
! 
! Purpose: Logical unit numbers and related variables
!
! Author: CCM Core Group
! 
!-----------------------------------------------------------------------

      common /comlun/ nsds    ,nrg     ,nrg2
      common /comlun/ ncid_ini,ncid_oz ,ncid_sst, ncid_trc
      common /comlun/ luhrest

      integer nsds       ! restart dataset unit
      integer nrg        ! master regeneration dataset unit
      integer nrg2       ! abs/ems regeneration dataset units
      integer ncid_ini   ! initial dataset
      integer ncid_oz    ! ozone dataset
      integer ncid_trc   ! greenhouse gas tracer dataset
      integer ncid_sst   ! sst dataset
      integer luhrest    ! history restart unit
# 30 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/inital.F90" 2
    include 'netcdf.inc'

    integer n
    character(256) locfn

    if (masterproc) then
        call getfil(ncdata, locfn)
        call wrap_open(locfn, NF_NOWRITE, ncid_ini)
    end if

    ! Li Dong: What is the difference between following modules?
    call initialize_prognostics
    call initialize_comfm1      !!(wh 2003.10.23)

    ! check for consistent settings on initial dataset
    call readinitial(ncid_ini)

    call timemgr_init

    !
    ! Initialize comslt and prognostics variables
    !
!!!   call initialize_prognostics
    !!   call initialize_comslt
    !
    ! Set commons
    !
    call initcom

    !
    ! Define physics data structures
    !
    call phys_grid_init









    !
    ! Initialize buffer, comsrf, and radbuffer variables
    ! (which must occur after the call to phys_grid_init)
    !
    call initialize_buffer
    call initialize_comsrf
    call initialize_radbuffer
    !
    ! Read in initial data
    !
    call read_inidat
    !
    ! Make all time levels of prognostics contain identical data.
    ! Fields to be convectively adjusted only *require* n3 time
    ! level since copy gets done in linems.
    !
    q31(:,:,:)       = q3(:,:,1,:,1)
    q32(:,:,:)       = q31(:,:,:)
    t31(:,:,:)       = t3(:,:,:,1)
    t32(:,:,:)       = t31(:,:,:)
    do n = 2, ptimelevels
        ps(:,:,n)     = ps(:,:,1)
        u3(:,:,:,n)   = u3(:,:,:,1)
        v3(:,:,:,n)   = v3(:,:,:,1)
        t3(:,:,:,n)   = t3(:,:,:,1)
        q3(:,:,:,:,n) = q3(:,:,:,:,1)
    end do

    return
end subroutine inital
