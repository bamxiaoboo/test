# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/physpkg.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/physpkg.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/physpkg.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/physpkg.F90" 2

!-----------------------------------------------------------------------
!
! Purpose:
! Loop over time, calling driving routines for physics
!
! Method:
! COUP_CSM and must be checked in order to invoke the proper calling
! sequence for running the CSM model
!
! Author:
! Original version:  CCM3
!-----------------------------------------------------------------------

subroutine physpkg(phys_state, phys_state0, gw,     ztodt,  &
                   phys_tend,  cldo,  cldn, tcwato, tcwatn, &
                   qcwato,     qcwatn,      lcwato, lcwatn)

    use shr_kind_mod,   only: r8 => shr_kind_r8
    use pmgrid,         only: plon, plat, masterproc
    use ppgrid,         only: pcols, pver
    use buffer,         only: pblht, tpert, qpert, qrs, qrl
    use comsrf



    use atm_lndMod,     only: atmlnd_drv


    use mpishorthand

    use phys_grid,      only: get_ncols_p, get_lat_all_p, get_lon_all_p
    use physics_types,  only: physics_state, physics_tend
    use comsrf
    use diagnostics,    only: diag_surf
    use time_manager,   only: get_nstep, is_first_step, is_first_restart_step, &
                              is_end_curr_month, get_curr_date
    use phys_buffer,    only: pbuf   ! added by SHI Xiangjun
    use CCPL_interface_mod
    use coupling_chemistry_model_mod, only:out_fld_for_coupling_chem, out_caculated_flds_for_coupling_chem

    use sst_data,       only: sst    ! added by SHI Xiangjun


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
# 50 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/physpkg.F90" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/comsol.h" 1
!
!	Common's to do with solar radiation
!
!	$Id: comsol.h,v 1.3 2000/06/02 16:20:40 jet Exp $
!
! Visible optical depth
!
      real(r8) tauvis     ! Visible optical depth

      common /comvis/ tauvis
!
! Solar constant
!
      real(r8) scon       ! Solar constant

      common /comsol/ scon
!
! Earth's orbital characteristics
!	
      real(r8) eccen       ! Earth's eccentricity factor (unitless) (typically 0 to 0.1)
      real(r8) obliq       ! Earth's obliquity angle (degree's) (-90 to +90) (typically 22-26)
      real(r8) mvelp       ! Earth's moving vernal equinox at perhelion (degree's) (0 to 360.0)
      integer iyear_AD ! Year (AD) to simulate above earth's orbital parameters for
!
! Orbital information after processed by orbit_params
!
      real(r8) obliqr      ! Earth's obliquity in radians
      real(r8) lambm0      ! Mean longitude of perihelion at the 
!                          ! vernal equinox (radians)
      real(r8) mvelpp      ! Earth's moving vernal equinox longitude
!                          ! of perihelion plus pi (radians)
!
      common /comorb/ eccen   , obliq   , mvelp   , obliqr  
      common /comorb/ lambm0  , mvelpp  , iyear_AD

# 51 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/physpkg.F90" 2

    real(r8), intent(in) :: gw(plat) ! Gaussian weights
    real(r8), intent(in) :: ztodt    ! physics time step unless nstep=0

    type(physics_state), intent(inout) :: phys_state(begchunk:endchunk)
    type(physics_state), intent(inout) :: phys_state0(begchunk:endchunk)  ! WAN Hui
    type(physics_tend ), intent(out  ) :: phys_tend(begchunk:endchunk)

    real(r8), intent(inout) :: cldo(pcols, pver, begchunk:endchunk)   ! old cloud
    real(r8), intent(inout) :: cldn(pcols, pver, begchunk:endchunk)   ! new cloud
    real(r8), intent(inout) :: tcwato(pcols, pver, begchunk:endchunk) ! old temperature
    real(r8), intent(inout) :: tcwatn(pcols, pver, begchunk:endchunk) ! new temperature
    real(r8), intent(inout) :: qcwato(pcols, pver, begchunk:endchunk) ! old moisture
    real(r8), intent(inout) :: qcwatn(pcols, pver, begchunk:endchunk) ! new moisture
    real(r8), intent(inout) :: lcwato(pcols, pver, begchunk:endchunk) ! cloud liquid water
    real(r8), intent(inout) :: lcwatn(pcols, pver, begchunk:endchunk) ! cloud liquid water

    integer i, m, lat, c, lchnk                ! indices
    integer lats(pcols)                        ! array of latitude indices
    integer lons(pcols)                        ! array of longitude indices
    integer ncol                               ! number of columns
    integer nstep                              ! current timestep number
    integer ncdate                             ! current date in integer format [yyyymmdd]
    integer ncsec                              ! current time of day [seconds]
    integer yr, mon, day                       ! year, month, and day components of a date

    real(r8) tsgave                            ! TS global average

    real(r8) tsgridpt_glob(plon,plat)          ! TS global summed at each grid point

    real(r8), save :: tsgridpt(plon,plat)      ! TS summed at each grid point
    real(r8), save :: tszonal(plat)            ! TS summed along each latitude
    integer,  save :: numts                    ! number of samples for monthly ave
    real(r8) timewtic(pcols,begchunk:endchunk)
    real(r8) timewtoc(pcols,begchunk:endchunk)

    !*** BAB's FV kludge
    ! DONG Li: check this out
    real(r8) tin(pcols, pver, begchunk:endchunk) ! input T, to compute FV output T

    !! (wanhui 2003.06.11)
    real(r8) dudtm, dvdtm, dtdtm
    integer  iic, kk

    call t_startf('physpkg_st')

    nstep = get_nstep()

    !-----------------------------------------------------------------------
    ! 1. Advance time information
    !-----------------------------------------------------------------------

    call advnce(phys_state)
    call t_stopf('physpkg_st')
    !
    !-----------------------------------------------------------------------
    ! 2. Calculate physical tendencies before flux coupler invokation
    !-----------------------------------------------------------------------
    !
    call t_startf('bc_physics')

!$OMP PARALLEL DO PRIVATE (C)
    do c = begchunk, endchunk

        call t_startf('tphysbc')
        call tphysbc(ztodt, pblht(1,c), tpert(1,c), srfflx_state2d(c)%ts, &
                     qpert(1,1,c), surface_state2d(c)%precl,              &
                     surface_state2d(c)%precc, surface_state2d(c)%precsl, &
                     surface_state2d(c)%precsc,                           &
                     srfflx_state2d(c)%asdir, srfflx_state2d(c)%asdif,    &
                     srfflx_state2d(c)%aldir, srfflx_state2d(c)%aldif,    &
                     snowhland(1,c),                                      &
                     qrs(1,1,c), qrl(1,1,c), surface_state2d(c)%flwds,    &
                     fsns(1,c),  fsnt(1,c),  flns(1,c),    flnt(1,c),     &
                     srfflx_state2d(c)%lwup,   surface_state2d(c)%srfrad, &
                     surface_state2d(c)%sols,  surface_state2d(c)%soll,   &
                     surface_state2d(c)%solsd, surface_state2d(c)%solld,  &
                     cldo(1,1,c), cldn(1,1,c),                            &
                     tcwato(1,1,c), tcwatn(1,1,c), qcwato(1,1,c),         &
                     qcwatn(1,1,c), lcwato(1,1,c), lcwatn(1,1,c),         &
                     phys_state(c), phys_tend(c),                         &
                     icefrac(1,c), landfrac(1,c), ocnfrac(1,c),           &
                     tin(1,1,c),                                          &
                     srfflx_state2d(c)%cflx(1,1),                         & ! added by WAN Hui, according to P Liu 2003
                     prcsnw(1,c),                                         & ! WAN Hui, according to P Liu 2003
                     phys_state0(c),                                      & ! WAN Hui, according to P Liu 2003



                     sst(1,c),                                            & ! added by SHI Xiangjun

                     pbuf)                                                  ! added by SHI Xiangjun

        call t_stopf ('tphysbc')

    end do

    call t_stopf ('bc_physics')


    ! Non-coupled mode
    !
    !-----------------------------------------------------------------------
    ! 3 Determine surface quantities - no flux coupler
    !-----------------------------------------------------------------------
    !
    ! Zero surface fluxes at beginning of each time step.  Land, ocean and
    ! ice processes will write into process specific flux variables.
    ! Subprocesses only work on grid points that have some fractional value
    ! for the subprocess type. (ie. camocn only works on grid points that have
    ! > 0. fraction ocean in that box.
    ! At the end of the time step these separate fluxes will be combined over the
    ! entire grid.
    !
    call srfflx_state_reset(srfflx_state2d)

    if (.not. aqua_planet) then
        !
        ! 3.1 Call land model driving routine
        !





        call t_startf('atmlnd_drv')
        call atmlnd_drv(nstep, iradsw, eccen, obliqr, lambm0, mvelpp, surface_state2d, srfflx_parm2d)
        call t_stopf ('atmlnd_drv')

        call update_srf_fluxes(srfflx_state2d, srfflx_parm2d, landfrac)
    end if
    !
    ! 3.2 Set ocean surface quantities - ocn model internal to atm
    !





    call t_startf('camoce')
    call camoce(surface_state2d, srfflx_parm2d)
    !
    ! Setup temp diagnostic variable that can be used to time weight
    ! those boxes that are operated on only for a portion of the run.
    !
    call t_startf('physpkg_st')
    timewtoc(:,:) = 0.
    do lchnk = begchunk, endchunk
        do i = 1, pcols
            if (ocnfrac(i,lchnk) > 0.) timewtoc(i,lchnk) = 1.
        end do
    end do
    call t_stopf('physpkg_st')
    call t_stopf('camoce')
    call update_srf_fluxes(srfflx_state2d, srfflx_parm2d, ocnfrac)
    !
    ! 3.3 Set ice surface quantities - icn model internal to atm
    !
    call t_startf('camice')
    call camice(surface_state2d, srfflx_parm2d)
    call t_stopf('camice')
    call t_startf('physpkg_st')
    timewtic(:,:) = 0.
    do lchnk = begchunk, endchunk
        do i = 1, pcols
            if (icefrac(i,lchnk) > 0.) timewtic(i,lchnk) = 1.
        end do
    end do
    call t_stopf('physpkg_st')
    call update_srf_fluxes(srfflx_state2d, srfflx_parm2d, icefrac)


# 278 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/physpkg.F90"
    !
    !-----------------------------------------------------------------------
    ! 4. Calculate physical tendencies after calling of flux coupler
    !    Not necessary at terminal timestep.
    !-----------------------------------------------------------------------
    !
    call t_startf ('ac_physics')
!$OMP PARALLEL DO PRIVATE (C, NCOL)
    do c = begchunk, endchunk
        ncol = get_ncols_p(c)
        !
        ! 4.1 Surface diagnostics for history files
        !
        call diag_surf(c, ncol,                                                   &
            srfflx_state2d(c)%shf, srfflx_state2d(c)%lhf, srfflx_state2d(c)%cflx, & ! surface flux
            srfflx_state2d(c)%tref, trefmxav(1,c), trefmnav(1,c),                 & ! temperature at 2m height
            srfflx_state2d(c)%qref,                                               & ! specific humidity at 2m height ! For FGOALS2.0
            srfflx_state2d(c)%rhref, phys_state(c)%ps,                            & ! relative humidity at 2m height ! For FGOALS2.0
            srfflx_state2d(c)%wsx, srfflx_state2d(c)%wsy,                         & ! wind stress
            icefrac(1,c), ocnfrac(1,c), landfrac(1,c),                            & ! fractions
            surface_state2d(c)%tssub, tsnam, srfflx_state2d(c)%ts,                & ! surface/subsurface temperature
            sicthk(1,c), snowhland(1,c), snowhice(1,c))                             ! surface snow

        ! 4.2
        call t_startf('tphysac')
        call tphysac(ztodt, pblht(1,c), qpert(1,1,c), tpert(1,c), srfflx_state2d(c)%shf,    &
                     srfflx_state2d(c)%wsx, srfflx_state2d(c)%wsy, srfflx_state2d(c)%cflx, sgh(1,c), &
                     srfflx_state2d(c)%lhf, landfrac(1,c), snowhland(1,c), srfflx_state2d(c)%tref,   &
                     surface_state2d(c)%precc, surface_state2d(c)%precl, tin(1,1,c), phys_state(c),  &
                     phys_tend(c), ocnfrac(1,c))
        call t_stopf('tphysac')
    end do
    call t_stopf('ac_physics')

    !-----------------------------------------------------------------------
    ! Calculate the monthly averaged TS
    ! NOTE: the following is only valid if restart on month boundary
    ! Initialize partial sums of global avg ts to 0.
    ! Sum TS pointwise for this timestep and save for monthly ave TS.
    !
    !-----------------------------------------------------------------------
    !
    call t_startf('global_ts')
    if (is_first_step() .or. is_first_restart_step()) then
        tsgridpt(:,:) = 0.
        numts = 0
    end if

    do c = begchunk, endchunk
        ncol = get_ncols_p(c)
        call get_lat_all_p(c, ncol, lats)
        call get_lon_all_p(c, ncol, lons)
        do i = 1, ncol
            tsgridpt(lons(i),lats(i)) = tsgridpt(lons(i),lats(i))+srfflx_state2d(c)%ts(i)*gw(lats(i))
        end do
    end do

    numts = numts+1     ! Increment number of time samples

    if (is_end_curr_month()) then






        call mpisum(tsgridpt, tsgridpt_glob, plon*plat, mpir8, 0, mpicom)
        if (masterproc) tsgridpt(:,:) = tsgridpt_glob(:,:)

        if (masterproc) then
            call get_curr_date(yr, mon, day, ncsec)
            ncdate = yr*10000+mon*100+day
            tsgave = 0.0
            do lat = 1, plat
                tszonal(lat) = 0.
                do i = 1, plon
                    tszonal(lat) = tszonal(lat)+tsgridpt(i,lat)
                end do
            end do
            do lat = 1, plat
                tsgave = tsgave+tszonal(lat)
            end do
            if (numts.gt.0) tsgave = tsgave/(2.*plon*numts)
            write(6, "('Notice: At the end of month ', I8.8, ' ', I5.5, ', ')", advance="no") ncdate, ncsec
            write(6, "('averaged Ts of ', I, ' samples is ', F)") numts, tsgave
        endif
        tszonal(:) = 0.
        numts = 0
    end if

    call t_stopf ('global_ts')

    call out_fld_for_coupling_chem('CLDF',cldn)
    call out_fld_for_coupling_chem('FROCEAN',ocnfrac)
    !call out_fld_for_coupling_chem('SLP',psl)
    do c = begchunk, endchunk
        call out_fld_for_coupling_chem('EFLUX',srfflx_state2d(c)%lhf(:),c)
        call out_fld_for_coupling_chem('HFLUX',srfflx_state2d(c)%lhf(:),c)
        call out_fld_for_coupling_chem('EVAP',srfflx_state2d(c)%lhf(:),c)
        call out_fld_for_coupling_chem('TS',srfflx_state2d(c)%lhf(:),c)
        call out_fld_for_coupling_chem('TSKIN',srfflx_state2d(c)%lhf(:),c)
        call out_fld_for_coupling_chem('PS',srfflx_state2d(c)%lhf(:),c)
        call out_fld_for_coupling_chem('PARDR',srfflx_state2d(c)%lhf(:),c)
        call out_fld_for_coupling_chem('PARDF',srfflx_state2d(c)%lhf(:),c)
    end do
    call out_caculated_flds_for_coupling_chem

    return
end subroutine physpkg
