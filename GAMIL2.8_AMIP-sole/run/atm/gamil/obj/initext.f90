# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/initext.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/initext.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/initext.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/initext.F90" 2

!-----------------------------------------------------------------------
!
! Purpose:
!
!   Initialize external models and/or boundary dataset information
!
! Method:
!
! Author:
!
!   CCM Core Group
!
!-----------------------------------------------------------------------

subroutine initext
!!(wh 2003.12.27)

    use shr_kind_mod,   only: r8 => shr_kind_r8
    use pmgrid
    use ppgrid,         only: begchunk, endchunk
    use phys_grid,      only: get_ncols_p, get_rlat_all_p, get_rlon_all_p,get_lat_all_p, get_lon_all_p
    use comsrf
    use rgrid
    use shr_orb_mod
    use ioFileMod
    use so4bnd
    use so4bnd_IPCC ! added by WAN Hui
    use commap

    use ice_constants,  only: Tffresh

    use filenames,      only: bndtvo, bndtvs
    use physconst,      only: stebol
    use time_manager,   only: is_first_step, is_perpetual, &
                              get_curr_calday, get_curr_date, get_perp_date

    use mpishorthand





    use atm_lndMod,     only: atmlnd_ini

    use sst_data,       only: sstini, sstint, sstan, sst
    use ice_data,       only: iceini, iceint



    implicit none


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
# 56 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/initext.F90" 2

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
# 57 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/initext.F90" 2

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

# 58 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/initext.F90" 2
    include 'netcdf.inc'


    integer  i                  ! indices
    integer  ncol               ! number of columns in current chunk
    real(r8) coszrs(pcols)      ! Cosine solar zenith angle
    real(r8) clat1(pcols)       ! Current latitude(radians)
    real(r8) clon1(pcols)       ! Current longitude(radians)
    integer  sghid              ! NetCDF sgh field id
    logical  oro_hires          ! true => ORO came from high res topo file
    logical  log_print          ! Flag to print out log information or not
    integer  ret                ! NetCDF returned status
    integer  attlen             ! NetCDF attribute length
    character(256) text         ! NetCDF attribute

    character(256) locfn        ! netcdf local filename to open
    character(4) ncnam(5)
    integer  yr, mon, day, tod  ! components of a date
    real(r8) calday             ! current calendar day
    integer  lchnk
    integer  lats(pcols)
    integer  lons(pcols)
    real(r8) tssav(pcols,begchunk:endchunk) ! cam surface temperatures

    calday = get_curr_calday()
    !
    !----------------------------------------------------------------------
    ! 1. Obtain datasets
    !----------------------------------------------------------------------
    !
    ! Obtain time-variant ozone and sst datatsets and do initial read of
    ! ozone dataset
    !
    if (.not. ideal_phys) then
        if (masterproc) then
            call getfil(bndtvo, locfn)
            call wrap_open(locfn, 0, ncid_oz)
            write(6, "('Notice: initext: ')", advance="no")
            write(6, "('wrap_open returns ncid ', I5)", advance="no") ncid_oz
            write(6, "(' for file ', A)") trim(locfn)
        end if

        if (.not. aqua_planet) then
            if (masterproc) then
                call getfil(bndtvs, locfn)
                call wrap_open(locfn, 0, ncid_sst)
                write(6, "('Notice: initext: ')", advance="no")
                write(6, "('wrap_open returns ncid ', I5)", advance="no") ncid_sst
                write(6, "(' for file ', A)") trim(locfn)
            end if
        end if

        call oznini
    end if
    !
    !----------------------------------------------------------------------
    ! 2. Obtain sulfate aerosol datasets
    !----------------------------------------------------------------------
    !
    if (doRamp_so4) then
        call sulfini
    end if

    if (doIPCC_so4) then
        call sulfini_IPCC ! added by WAN Hui
    end if


    !
    !----------------------------------------------------------------------
    ! 3. Determine if SGH field came from hi-res dataset
    !----------------------------------------------------------------------
    !
    if (is_first_step()) then
        if (masterproc) then
            call wrap_inq_varid(ncid_ini, 'SGH', sghid)
            ret = nf_inq_attlen(ncid_ini, sghid, 'from_hires', attlen)
            if (ret == nf_noerr .and. attlen > 256) then
                write(6, "('Error: initext: attribute length of ""from_hires"" is too long')")
                call endrun
            end if
            ret = nf_get_att_text(ncid_ini, sghid, 'from_hires', text)
            if (ret == nf_noerr .and. text(1:4) == 'true') then
                oro_hires = .true.
                write(6, "('Notice: initext: attribute ""from_hires"" is true')")
                write(6, "('Notice: initext: ""tssub"" will be used to guess sea ice')")
            else
                oro_hires = .false.
                write(6, "('Notice: initext: attribute ""from_hires"" is either false or not present')")
                write(6, "('Notice: initext: where sea ice exists, its initial temperature will be just below freezing')")
            end if
        end if

        call mpibcast(oro_hires, 1, mpilog, 0, mpicom)

    end if
    !
    !----------------------------------------------------------------------
    ! 4. Setup the characteristics of the orbit (Based on the namelist parameters)
    !----------------------------------------------------------------------
    !
    if (masterproc) then
        log_print = .true.
    else
        log_print = .false.
    end if
    call shr_orb_params(iyear_AD, eccen, obliq , mvelp, obliqr, lambm0, mvelpp, log_print)
    !
    !----------------------------------------------------------------------
    ! 5. Initialize land model
    !----------------------------------------------------------------------
    !
    ! This involves initializing land albedos, surface temperature, lwup and snowh.
    ! NOTE: On restart, lwup, ts, albedos and snowh, come from the atm restart data.
    !
    if (is_first_step()) then
        call srfflx_state_reset(srfflx_state2d)
    end if
    if (.not. adiabatic .and. .not. ideal_phys .and. .not. aqua_planet) then
        call atmlnd_ini(srfflx_parm2d)
    end if
    !
    ! Save off ts here because it is needed below for calculating lwup.  The
    ! updated state values are summed according to the fractional area of the
    ! underlying surface and only represent an actual grid box value after the
    ! last surface process has been called. TS is a special case as it is
    ! calculated from lwup and overwritten each time update surface
    ! fluxes is called.  The intermediate ts values returned from the update
    ! routine are wrong until ts is calculated from the full gridbox value of lwup
    ! lwup is only complete after the last surface process is called or if we
    ! are calculating a grid point that is all land.  We will save the
    ! intermediate value returned from land for our calculations below.
    !
    do lchnk = begchunk, endchunk
        tssav(:,lchnk) = srfflx_parm2d(lchnk)%ts(:)
    end do
    ! LIU Li: only call update_srf_fluxes at initial run
    if (is_first_step()) then
        call update_srf_fluxes(srfflx_state2d, srfflx_parm2d, landfrac)
    end if

    !----------------------------------------------------------------------
    ! 6. Initialize ocean and ice model
    !----------------------------------------------------------------------
# 210 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/initext.F90"
    !
    ! Data ocean model: Initialize ocean/sea-ice surface datasets and determine initial sea surface
    ! temperature
    !
    if (.not. adiabatic .and. .not. ideal_phys) then
        call sstini
        call iceini
        call sstint
        call iceint
    else
        icefrac(:pcols,begchunk:endchunk) = 0.0
        call update_srf_fractions
    end if
    !
    !----------------------------------------------------------------------
    ! 7. Initialize surface and sub-surface temperatures, set new sea ice
    !    concentrations and compute longwave up over non-land
    !----------------------------------------------------------------------
    !
    if (is_first_step()) then
        do lchnk = begchunk, endchunk
            if (.not. adiabatic .and. .not. ideal_phys) then
                ncol = get_ncols_p(lchnk)
                do i = 1, ncol
                    srfflx_state2d(lchnk)%ts(i) = &
                        landfrac(i,lchnk)*tssav(i,lchnk) + &
                        icefrac(i,lchnk)*tsice(i,lchnk) + &
                        ocnfrac(i,lchnk)*(sst(i,lchnk)+Tffresh)
                    if (landfrac(i,lchnk).ne.1.) then
                        srfflx_state2d(lchnk)%lwup(i) = &
                            stebol*(srfflx_state2d(lchnk)%ts(i)**4)
                    end if
                end do
            end if
        end do
    end if

    !
    !----------------------------------------------------------------------
    ! 8. Initialize non-land albedos at NSTEP = 0.  At NSTEP = 1 and
    !    beyond, albedos will be computed for the *next* timestep to
    !    accomodate coupling with a single interface.
    !----------------------------------------------------------------------
    !
    if (is_first_step()) then
        do lchnk = begchunk, endchunk
            ncol = get_ncols_p(lchnk)
            call get_rlat_all_p(lchnk, ncol, clat1)
            call get_rlon_all_p(lchnk, ncol, clon1)
            call zenith(calday, clat1, clon1, coszrs, ncol)
            call albocean(lchnk, ncol, coszrs, &
                          srfflx_parm2d(lchnk)%asdir, srfflx_parm2d(lchnk)%aldir, &
                          srfflx_parm2d(lchnk)%asdif, srfflx_parm2d(lchnk)%aldif)
        end do

        call update_srf_fluxes(srfflx_state2d, srfflx_parm2d, ocnfrac)

        do lchnk = begchunk, endchunk
            ncol = get_ncols_p(lchnk)
            call get_lat_all_p(lchnk, ncol, lats)
            call get_lon_all_p(lchnk, ncol, lons)
            call get_rlat_all_p(lchnk, ncol, clat1)
            call get_rlon_all_p(lchnk, ncol, clon1)
            call zenith (calday, clat1, clon1, coszrs, ncol)
            call albice(lchnk, ncol, tsice(1,lchnk), snowhice(1,lchnk), coszrs, &
                        srfflx_parm2d(lchnk)%asdir, srfflx_parm2d(lchnk)%aldir, &
                        srfflx_parm2d(lchnk)%asdif, srfflx_parm2d(lchnk)%aldif)
            !
            ! fill in ice albedoes for therm ice model
            !
            asdirice(:ncol,lchnk)= srfflx_parm2d(lchnk)%asdir(:ncol)
            aldirice(:ncol,lchnk)= srfflx_parm2d(lchnk)%aldir(:ncol)
            asdifice(:ncol,lchnk)= srfflx_parm2d(lchnk)%asdif(:ncol)
            aldifice(:ncol,lchnk)= srfflx_parm2d(lchnk)%aldif(:ncol)
        end do
        call update_srf_fluxes(srfflx_state2d,srfflx_parm2d,icefrac)
    end if









    return
end subroutine initext
