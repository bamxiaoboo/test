# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/stepon.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/stepon.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/stepon.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/stepon.F90" 2

!-----------------------------------------------------------------------
!
! Purpose:
! Loop over time, calling driving routines for physics, dynamics,
! transport
!
! Method:
!
! Author:
! Original version:  CCM1
! Standardized:      J. Rosinski, June 1992
! Reviewed:          B. Boville, D. Williamson, August 1992
! Reviewed:          B. Boville, D. Williamson, April 1996
! Restructured:      J. Truesdale, May 1999
!
!-----------------------------------------------------------------------

subroutine stepon

    use shr_kind_mod,   only: r8 => shr_kind_r8
    use history,        only: wshist, wrapup
    use pmgrid
    ! DONG Li: What is "rgrid" for?
    use rgrid
    use prognostics
    use comfm1
    use buffer
    use restart,        only: write_restart




    use ppgrid,         only: begchunk, endchunk
    use physics_types,  only: physics_state, physics_tend
    use dp_coupling,    only: d_p_coupling, p_d_coupling
    use commap
    use physconst,      only: gravit
    use time_manager,   only: advance_timestep, get_step_size, get_nstep, &
                              is_first_step, is_first_restart_step, &
                              is_last_step, is_end_curr_day, get_curr_calday, &
                              dtdy ! added by WANG Hui
    use CCPL_interface_mod

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
# 50 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/stepon.F90" 2

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
 
# 51 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/stepon.F90" 2

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
# 52 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/stepon.F90" 2

    type(physics_state), allocatable :: phys_state(:)
    type(physics_state), allocatable :: phys_state0(:) ! added by WAN Hui, according to P Liu 2003)
    type(physics_tend ), allocatable :: phys_tend(:)

    real(r8), allocatable :: t2(:,:,:) ! temp tendency
    real(r8), allocatable :: fu(:,:,:) ! u wind tendency
    real(r8), allocatable :: fv(:,:,:) ! v wind tendency

    real(r8) rpmid(plond,plev)
    real(r8) pdel(plond,plev)
    real(r8) pint(plond,plevp)
    real(r8) pmid(plond,plev)
    real(r8) dtime               ! timestep size  (physics package)
    real(r8) ztodt               ! twice time step unless nstep=0
    real(r8) wcstart, wcend      ! wallclock timestamp at start, end of timestep
    real(r8) usrstart, usrend    ! user timestamp at start, end of timestep
    real(r8) sysstart, sysend    ! sys timestamp at start, end of timestep

    real(r8) calday              ! current calendar day
    real(r8) dsghl(plev)
    integer nseq

    integer i, k, lat, j, begj   ! longitude,level,latitude indices
    !
    ! Externals
    !
    logical, external :: rstwr  ! whether or not to write restart files
    !
    !-----------------------------------------------------------------------
    call t_startf('stepon_startup'); if(masterproc) write(6,*) '+++++ stepon_startup +++++'
    dtime = get_step_size();         if(masterproc) write(6,*) 'dtime = ', dtime
                                     if(masterproc) write(6,*) 'dtdy  = ', dtdy
    nseq  = dtime/(dtdy-0.01);       if(masterproc) write(6,*) 'nseq  = ', nseq
    !!
    !! fm2003 : calculate dsghl for subroutine 'avnegq' at the end of 'qpdata'
    !!
    dsghl(1) = 0.0
    do k=2,plev
        dsghl(k) = dsig(k-1)/dsig(k)
    enddo

    if (masterproc) write(6, "('Notice: stepon: dsghl set')")

    pmtop = pmtop*0.01d0

    ! WAN Hui 2003.07.08)

    if (is_first_step()) then
        if(masterproc) write(6, "('Notice: stepon: first step start')")
        itime = 0
        !
        ! Calculate vertical motion field
        !
        omga(:,:,:) = 0.0
        if(masterproc) write(6, "('Notice: stepon: set omga to zero')")
        call init_ac_switching(pmtop) ! added by WAN Hui 2003.10.28
    else if (is_first_restart_step()) then
        !sq(:,:,:) = 0.0
        ! DONG Li: clean this out
        if(masterproc) write(6, "('Notice: stepon: sq set to 0.0 in comfm1 (Please clarify this)')")
    else
        if(masterproc) write(*, "('Error: neither first_step nor first_restart_step')")
        call endrun
    end if

    allocate(phys_state(begchunk:endchunk))
    allocate(phys_state0(begchunk:endchunk)) ! added by WAN Hui
    allocate(phys_tend(begchunk:endchunk))
    allocate(t2(plond,plev,beglat:endlat))
    allocate(fu(plond,plev,beglat:endlat))
    allocate(fv(plond,plev,beglat:endlat))
    !
    ! Beginning of basic time step loop
    !
    call t_stopf ('stepon_startup')

    ! Begin time loop.

    do

        call t_startf('stepon_st')
        if (masterproc .and. print_step_cost) then
            call t_stampf(wcstart, usrstart, sysstart)
        end if

        ! DONG Li: clarify this
        !ztodt = 2.0*dtime
        ztodt = dtime

        calday = get_curr_calday()

        if (masterproc) then
            write(6, *)
            write(6, *) 'date:', calday
        end if

        !----------------------------------------------------------
        ! PHYSPKG  Call the Physics package
        !----------------------------------------------------------
        if (masterproc) write(6, *) '------physpkg------'

        begj = beglatex+numbnd

        call t_stopf('stepon_st')
        call t_startf('d_p_coupling')
        call d_p_coupling(ps(1,beglat,n3m2), t3(i1,1,begj,n3m2), u3(i1,1,begj,n3m2), &
                          v3(i1,1,begj,n3m2), q3(i1,1,1,begj,n3m2), &
                          q31(i1,1,begj), t31(i1,1,begj), q32(i1,1,begj), t32(i1,1,begj),&
                          omga, phis, phys_state)
        call t_stopf('d_p_coupling')

        !!(wh, according to P Liu 2003)
        if (is_first_restart_step()) then
            call t_startf('d_p_coupling')
            call d_p_coupling(ps(1,beglat,n3m2), t3(i1,1,begj,n3m2), u3(i1,1,begj,n3m2), &
                              v3(i1,1,begj,n3m2), q3(i1,1,1,begj,n3m2), &
                              q31(i1,1,begj), t31(i1,1,begj), q32(i1,1,begj), t32(i1,1,begj),&!(ljli)
                              omga, phis, phys_state0)
            call t_stopf('d_p_coupling')
        else
            !for Tiedtke scheme
            call t_startf('d_p_coupling')
            call d_p_coupling(ps(1,beglat,n3), t3(i1,1,begj,n3), u3(i1,1,begj,n3), &
                              v3(i1,1,begj,n3), q3(i1,1,1,begj,n3), &
                              q31(i1,1,begj), t31(i1,1,begj), q32(i1,1,begj), t32(i1,1,begj),&!(ljli)
                              omga, phis, phys_state0)
            call t_stopf('d_p_coupling')
        end if
        !!(wh, according to P Liu 2003)

        call t_startf('phys_driver')

        if (masterproc) then
            write(6, *) 'ideal_phys =', ideal_phys
            write(6, *) 'adiabatic  =', adiabatic
        end if

        if (ideal_phys) then
            call phys_idealized(phys_state, phys_tend, ztodt, sigl)
        else if (adiabatic) then
            call phys_adiabatic(phys_state, phys_tend)
        else
            call physpkg( &
                phys_state, phys_state0, w, ztodt, phys_tend,     &
                cld(1,1,begchunk,n3m2),   cld(1,1,begchunk,n3),   &
                tcwat(1,1,begchunk,n3m2), tcwat(1,1,begchunk,n3), &
                qcwat(1,1,begchunk,n3m2), qcwat(1,1,begchunk,n3), &
                lcwat(1,1,begchunk,n3m2), lcwat(1,1,begchunk,n3))
        end if
        call t_stopf('phys_driver')

        call t_startf('p_d_coupling')
        call p_d_coupling(phys_state, phys_tend, t2, fu, fv, &
            qminus(i1,1,1,begj), q3(i1,1,1,begj,n3), q31(i1,1,begj), t31(i1,1,begj))
        call t_stopf('p_d_coupling')

        !----------------------------------------------------------
        ! DYNPKG Call the Dynamics Package
        !----------------------------------------------------------
        if (masterproc) write(6, *) '------dynpkg------'

        call t_startf('dynpkg')

        ! accumulate su, sv, st and update q

        call a_c_switching(fu, fv, t2, beglat, endlat)   !!(wh 2003.10.28)

        call dynpkg(dtdy, nseq, dsghl)        !!(wh 2003.10.23)

        ! prepare data for physics

!        call c_coupler_perturb_roundoff_errors

        call c_a_switching(pmtop)            !!(wh 2003.10.28)

        call t_stopf('dynpkg')
        !
        ! Shift time pointers
        !
        call shift_time_indices

        call t_startf('stepon_st')
        if (is_first_restart_step()) then
            call print_memusage
        end if

        ! Set end of run flag.


        if (is_last_step()) nlend = .true.






        !
        !----------------------------------------------------------
        ! History and restart logic: Write and/or dispose history tapes if required
        !----------------------------------------------------------
        !
        call t_startf ('wshist')
        call wshist ()
        call t_stopf ('wshist')
        !
        ! Write restart file
        !
        if (rstwr() .and. nrefrq /= 0) then
            call t_startf ('write_restart')
            call write_restart
            call t_stopf ('write_restart')
        end if
        !
        ! Dispose necessary files
        !
        call t_startf ('wrapup')
        call wrapup
        call t_stopf ('wrapup')

        if (masterproc .and. print_step_cost) then
            call t_stampf (wcend, usrend, sysend)
            write(6,'(a,3f8.3,a)')'Prv timestep wallclock, usr, sys=', &
                wcend-wcstart, usrend-usrstart, sysend-sysstart, ' seconds'
        end if
        !
        ! Advance timestep before returning to top of loop
        !
!        call CCPL_advance_timer()
        call advance_timestep()
        call t_stopf('stepon_st')
        !
        ! Check for end of run
        !
        if (nlend) then
            deallocate(phys_state)
            deallocate(phys_state0)   !!(wh)
            deallocate(phys_tend)
            deallocate(t2)
            deallocate(fu)
            deallocate(fv)



            return
        end if

    end do  ! End of timestep loop

end subroutine stepon
