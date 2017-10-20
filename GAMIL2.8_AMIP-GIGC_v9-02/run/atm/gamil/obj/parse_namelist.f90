# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/parse_namelist.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/parse_namelist.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/parse_namelist.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/parse_namelist.F90" 2

subroutine parse_namelist

    !! (wh 2003.04.29)
    !! (wh 2003.07.21)
    !! (wh 2003.12.27)
    !! (wh 2004.04.14)
    !! (wh 2004.12.17)
    !! (wh 2005.01.28)
    !-----------------------------------------------------------------------
    !
    ! Purpose:
    ! Read data from namelist atmexp to define the run. Process some of the
    ! namelist variables to determine history and restart/branch file path
    ! names.  Check input namelist variables for validity and print them
    ! to standard output.
    !
    ! Method:
    ! Important Note for running on SUN systems: "implicit automatic (a-z)"
    ! will not work because namelist data must be static.
    !
    ! Author:
    ! Original version:  CCM1
    ! Standardized:      L. Bath, June 1992
    !                    T. Acker, March 1996
    !
    !-----------------------------------------------------------------------
    !
    ! $Id: parse_namelist.F90,v 1.22.2.14 2002/09/17 05:36:54 erik Exp $
    !
    !-----------------------------------------------------------------------

    use shr_kind_mod, only: r8 => shr_kind_r8
    use infnan,       only: inf
    use pmgrid
    use history
    use comhd,        only: dfs0    !!(wh 2004.04.14)
    use shr_orb_mod
    use so4bnd_IPCC    !!(wh)
    use so4bnd
    use ramp_so4_mod
    use moistconvection, only: convection_scheme  !!(wh 2004.12.17)
    use units
    use tracers,      only: nusr_nad, nusr_adv
    use constituents, only: pcnst, ch4vmr, n2ovmr, f11vmr, f12vmr, co2vmr
    !! use time_manager, only: calendar, dtime, nestep, nelapse, &
    use time_manager, only: calendar, dtime, nestep, nelapse,dtdy, &   !!(2004.04.14)
        start_ymd, start_tod, stop_ymd, stop_tod, ref_ymd, ref_tod, &
        perpetual_run, perpetual_ymd, tm_aqua_planet
    use filenames, only: nrevsn, ncdata, bndtvs, bndtvo, absems_data, bndtvg, &
        mss_wpass, rest_pfile, mss_irt, caseid, init_filepaths, get_archivedir,&   !!(wh)
        bndtvaer             !! sxj-2009-0309                      !!(wh 2005.01.28)
    use restart, only: set_restart_filepath

    use ice_dh, only: prognostic_icesnow,reset_csim_iceprops, icemodel_is


    implicit none


# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/comadj.h" 1
!
! $Id: comadj.h,v 1.6 2001/08/10 22:07:55 boville Exp $
! $Author: boville $
!
!
! Convective adjustment
!
      common/comadj/ nlvdry
!
      integer nlvdry        ! Number of levels to apply dry adjustment
!

 
# 63 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/parse_namelist.F90" 2

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
# 64 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/parse_namelist.F90" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/comtfc.h" 1
!----------------------------------------------------------------------- 
! 
! Purpose: time filter coefficient
! 
!-----------------------------------------------------------------------
      common /comtfc/ eps

      real(r8) eps             ! Time filter coefficient
 
# 65 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/parse_namelist.F90" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/perturb.h" 1
!----------------------------------------------------------------------- 
! 
! Purpose: Perturbation information
! 
! Author: Jim Rosinski
! 
!-----------------------------------------------------------------------
      common /perturb/ pertlim
      real(r8) pertlim        ! Bound for rectangular distribution of perturbation
!
 
# 66 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/parse_namelist.F90" 2

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

# 67 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/parse_namelist.F90" 2

    !-----------------------------------------------------------------------
    include 'netcdf.inc'
    !
    !---------------------------Local variables-----------------------------
    !
    logical linebuf
    character(len=256) :: archive_dir = ''
    !






    data linebuf/.false./ ! Default: allow system to buffer stdout
    !
    character ctemp*8      ! Temporary character strings
    integer ntspdy         ! number of timesteps per day
    integer t              ! history tape index
    integer lastchar       ! index to last char of a char variable
    integer ierr           ! error code
    integer klen           !!(wh)




    !
    !-----------------------------------------------------------------------
    !
    ! ALPHABETICAL listing of variables in the atmexp namelist:
    !
    ! variable                description
    ! --------             -----------------
    !
    ! calendar             Calendar to use in date calculations.  'no_leap' (default) or 'gregorian'
    !
    ! ctitle               Case title for header.
    !
    ! bndtvs               Path and filename of time-variant boundary
    !                      dataset for sst's.
    !
    ! bndtvg               Path and filename of time-variant boundary
    !                      dataset for greenhouse loss rates.
    !		       (required if trace_gas is set to true)
    !
    ! bndtvo               Path and filename of time-variant boundary
    !                      dataset for ozone.
    !
    ! absems_data          Dataset with absorption and emissivity factors.
    !
    ! caseid               Case name for model run.  32 characters max.
    !                      Included in mass store path name for history and
    !                      restart files.
    !
    ! dif2 = nnn.n,        del2 horizontal diffusion coeff. Defaults
    !                      to 2.5e5.
    !
    ! dif4 = nnn.n,        del4 horizontal diffusion coeff. Defaults
    !                      to 1.e16.
    !!
    !! dfs0 = nn.n         horizontal diffusion coeff. Defaults to 0.02
    !!
    !
    ! divdampn = 0.        Number of days (from nstep 0) to run divergence
    !                      damper
    !
    ! dtime = nnnn,        Model time step in seconds. Default is dycore dependent.
    !!
    !! dtdy = nnn.n,       time step of the dynamical core of LASG  !!(wh 2004.04.14)
    !!
    !
    ! eccen                The eccentricity of the earths orbit to use (1.e36 to
    !		       use the default -- defined as SHR_ORB_UNDEF_REAL).
    !                      (Unitless typically 0 - 0.1)
    !
    ! eps = nnn.n,         time filter coefficient. Defaults to 0.06.
    !
    ! fincl1 = 'field1', 'field2',...
    !                      List of fields to add to the primary history file.
    !
    ! fincl[2..6] = 'field1', 'field2',...
    !                      List of fields to add to the auxiliary history file.
    !
    ! fexcl1 = 'field1','field2',...
    !                      List of field names to exclude from default
    !                      primary history file (default fields on the
    !                      Master Field List).
    !
    ! fexcl[2..6] = 'field1','field2',...
    !                      List of field names to exclude from
    !                      auxiliary history files.
    !
    ! fhstpr1 = 'field1', 'field2',...
    !                      List of fields to change buffer size in
    !                      primary history file
    !
    ! fhstpr[2..6] = 'field1', 'field2',...
    !                      List of fields to change buffer size in auxiliary files
    !
    ! fwrtpr1 = 'field1', 'field2',...
    !                      List of fields to change output data type in
    !                      primary history file
    !
    ! fwrtpr[2..6] = 'field1', 'field2',...
    !                      List of fields to change output data type in
    !                      auxiliary files
    !
    ! iradae = nnn,        frequency of absorp/emis calc in time steps
    !                      (positive) or hours (negative).
    !
    ! iradlw = nnn,        frequency of longwave rad. calc. in time steps
    !                      (positive) or hours (negative).
    !
    ! iradsw = nnn,        freq. of shortwave radiation calc in time steps
    !                      (positive) or hours (negative).
    !
    ! mss_irt              Mass Store retention time for history files
    !                      in days.
    !
    ! itsst = nnn,         frequency of SST update in time steps
    !
    ! kmxhdc = nn          number of levels (starting from model top) to
    !                      apply Courant limiter.  Defaults to 5.
    !
    ! mfilt = nn,nn,nn     Array containing the maximum number of time
    !                      samples per disk history file. Defaults to 5.
    !                      The first value applies to the primary hist. file,
    !                      the second to the first aux. hist. file, etc.
    !
    ! mvelp                The longitude of vernal equinox of the earths orbit to
    !		       use (1.e36 to use the default -- defined as
    !                      SHR_ORB_UNDEF_REAL).  (0-360 degrees')
    !
    ! ncdata               Path and filename of initial condition dataset.
    !
    ! nelapse = nnn,       Specify the ending time for the run as an interval
    !                      starting at the current time in either timesteps
    !                      (if positive) or days (if negative).
    !                      Either nestep or (stop_ymd,stop_tod) take precedence.
    !
    ! nestep = nnnn,       Specify the ending time for the run as an interval
    !                      starting at (start_ymd,start_tod) in either timesteps
    !                      (if positive) or days (if negative).
    !                      (stop_ymd,stop_tod) takes precedence if set.
    !
    ! nhtfrq = nn,nn,nn,.. Output history frequency for each tape
    !
    !                      If = 0 : monthly average
    !                      If > 0 : output every nhtfrq time steps.
    !                      If < 0 : output every abs(nhtfrq) hours.
    !
    ! nlvdry = nn,         Number of layers over which to do dry
    !                      adjustment. Defaults to 3.
    !
    ! nrefrq = nn,         Frequency of restart dataset writes.
    !                      For non-flux coupled runs, restart files are
    !                      written and disposed for every dispose of the
    !                      primary history file. If this variable is 0, then
    !                      no restart are written.
    !                      NOTE: NOW DUE TO NEW LSM: THIS VARIABLE CAN
    !                      ONLY BE 1 or 0.
    !                      For flux coupled runs, insist that restart files
    !                      are written
    !
    ! nrevsn               Filename of dataset to branch from (nsrest=3)
    !                      Full pathname of dataset required.
    !
    !------------------------------------------------------------------
    ! The following 5 are specific to f-v dynamics (see dynpkg for info)
    !------------------------------------------------------------------
    ! nsplit               Lagrangian time splits for Lin-Rood.
    ! iord                 scheme to be used for E-W transport (default: 4)
    ! jord                 scheme to be used for N-S transport (default: 4)
    ! kord                 scheme to be used for vertical mapping (default: 4)
    ! use_eta              flag to use ETA values from dynamics/lr/set_eta.F90
    !                      Default is .false. (use eta values from IC)
    !
    ! nsrest               Code for type of run: 0=initial, 1=restart,
    !                      or 3=branch
    !
    ! archive_dir          Archive directory name
    !
    ! hfilename_spec       Flexible filename specifier for history files
    !
    ! rest_pfile           Name of Restart Pointer file
    !
    ! mss_wpass            Write password for model output files.
    !
    ! ozncyc = .T.,        If false, do not cycle ozone dataset(assume
    !                      multiyear)
    !
    ! obliq                The obliquity of the earths orbit to use (1.e36 to
    !		       use the default -- defined as SHR_ORB_UNDEF_REAL).
    !                      (Degree's)
    !
    ! perpetual_run = .F.  Set to .true. to specify that the run will use a perpetual
    !                      calendar.  If perpetual_ymd is not set then read the perpetual
    !                      date from the initial file.
    !
    ! perpetual_ymd        Perpetual date specified as (year*1000 + month*100 + day).
    !                      This date overrides the date from the initial file.
    !                      If aqua_planet=.true. then perpetual_ymd is ignored and the
    !                      perpetual date is set to 321.
    !
    ! pertlim = n.n        Max size of perturbation to apply to initial
    !		       temperature field.
    !
    ! ref_ymd              Reference date for time coordinate encoded in yearmmdd format.
    !                      Default value is start_ymd.
    !
    ! ref_tod              Reference time of day for time coordinate in seconds since 0Z.
    !                      Default value is start_tod.
    !
    ! sstcyc = .T.,        If false, do not cycle sst dataset(assume
    !                      multiyear)
    !
    ! logical reset_csim_iceprops = .F.,
    !
    !                    ! if true => resets the csim ice properties to base state
    !                    ! No Snow Cover, TSICE and TS1-4 are all set to
    !                    ! freezing. Default is false.
    !                    ! The csim is sensitive to imbalances between the
    !                    ! surface temperature and ice temperatures. When
    !                    ! using an initial conditions dataset interpolated
    !                    ! from a different resolution you may have to set this
    !                    ! to true to get csim to run.  If set to true you will
    !                    ! have to allow time for the ice to "spin-up".
    !
    ! start_ymd            Starting date for run encoded in yearmmdd format.  Default value
    !                      is read from initial conditions file.
    !
    ! start_tod            Starting time of day for run in seconds since 0Z.  Default value
    !                      is read from initial conditions file.
    !
    ! stop_ymd             Stopping date for run encoded in yearmmdd format.  No default.
    !
    ! stop_tod             Stopping time of day for run in seconds since 0Z.  Default: 0.
    !
    ! adiabatic = .F.      Don't call physics
    !
    ! ideal_phys = .F.     Only run the "idealized" dynamical core
    !                      (dynamics + specified physics) of the model.
    !
    ! aqua_planet = .F.    Run in "aqua_planet" mode.  Physics remains on but is run for
    !                      perpetual vernal equinox conditions; phis = 0; ocean
    !                      everywhere - no land and no sea-ice; SST's specified analytically
    !
    ! flxave = .T.         If true, only send data to the flux coupler on
    !                      radiation time steps. This namelist variable is
    !                      only used when running through the flux coupler.
    !
    ! nusr_adv = nnn       Number of user defined advected tracers. Defaults to 0.
    !
    ! nusr_nad = nnn       Number of user defined non-advected tracers. Defaults to 0.
    !
    ! precc_thresh         Precipitation threshold to use for PRECCINT and PRECCFRQ (mm/hr)
    !                      Defaults to 0.1.
    !
    ! precl_thresh         Precipitation threshold to use for PRECLINT and PRECLFRQ (mm/hr)
    !                      Defaults to 0.05.
    !
    ! trace_gas = .F.      If true, turn on greenhouse gas code for
    !                      CH4, N2O, CFC11 and CFC12 . (Must add 4 to pcnst)
    !
    ! trace_test1 = .F.    If true, implement trace test code with 1 tracer,
    !                      (radon)
    !
    ! trace_test2 = .F.    If true, implement trace test code with 2 tracers,
    !                      (radon and conserved unit tracer)
    !
    ! trace_test3 = .F.    If true, implement trace test code with 3 tracers,
    !                      (radon, conserved unit tracer and ozone-like tracer)
    !
    ! readtrace = .T.      If true, tracer initial conditions obtained from
    !                      initial file.
    !
    ! co2vmr               global       co2 volume mixing ratio
    ! ch4vmr               tropospheric ch4 volume mixing ratio
    ! n2ovmr               tropospheric n2o volume mixing ratio
    ! f11vmr               tropospheric f11 volume mixing ratio
    ! f12vmr               tropospheric f12 volume mixing ratio
    !
    ! iyear_AD   	       The year AD to calculate the orbital parameters for.
    !                      By default this is set to 2000000000 (defined to SHR_ORB_UNDEF_INT)
    !                      which means use the input values o: eccen, obliq and mvelp.
    !
    ! inithist             Generate initial dataset as auxillary history file
    !                      can be set to 'MONTHLY', 'YEARLY' or 'NONE'.
    !                      default: 'MONTHLY '
    !
    ! prognostic_icesnow = .T,  prognostic snow over ice, currently limited to
    !                      0.5m.  If this is false then a snow climatology
    !                      is used (default .T.)
    !
    ! tauvis               Visible optical depth (default .14)
    !
    ! linebuf              true => force buffer flush of stdout with each
    !                      newline generated (useful for debugging)
    !
    ! empty_htapes         true => no fields by default on history tapes
    !
    ! print_step_cost      true => print per timestep cost info
    !
    ! avgflag_pertape      A, I, X, or M means avg, instantaneous, max or min for all fields on
    !                      that tape
    !

    character(16) scenario_ghg
    !                    ! values can be 'FIXED' or 'RAMPED'
    !                    ! sets co2,ch4,n2o,cfcf11,cfc12 volume mixing ratios
    !                    ! FIXED => volume mixing ratios are fixed and are
    !                    ! either have preset or namelist input values
    !                    ! RAMPED => volume mixing ratios are ramped
    !                    ! DEFAULT: FIXED
    !
    character(16) scenario_so4
    !                    ! values can be 'FIXED' , 'RAMPED' or 'IPCC'
    !                    ! FIXED => zero sulfate except for background added
    !                    ! in aermix.F
    !                    ! RAMPED => sulfate is ramped
    !                    ! IPCC   => IPCC scenario ! added by WAN Hui
    !                    ! DEFAULT: FIXED
    !
    character(16) scenario_scon
    !                    ! values can be 'FIXED' or 'RAMPED'
    !                    ! FIXED => scon is fixed and can either have preset or
    !                    ! namelist value
    !                    ! RAMPED => scon is ramped
    !                    ! DEFAULT => FIXED
    !
    integer rampYear_ghg
    !                    ! ramped gases fixed at this year if set to a value
    !                    ! greater than zero.  Default value is 0.
    !
    integer rampYear_so4
    !                    ! ramped sulfate fixed at this year if set to a value
    !                    ! greater than zero.  Default value is 0.
    !
    integer rampYear_scon
    !                    ! ramped scon fixed at this year if set to a value
    !                    ! greater than zero.  Default value is 0.
    !
    !   logical indirect
    !                    ! true => include indirect radiative effects of
    !                    ! sulfate aerosols.  Default is false.
    !                    ! this setting is independent of the
    !                    ! setting of SCENARIO_SO4
    !
    character*256 sulfdata
    !                    ! Path and filename of time-variant sulfate dataset
    !                    ! MUST be set if SCENARIO_SO4 = 'RAMPED'
    !                    ! NOT USED if SCENARIO_SO4 = 'FIXED'
    !
    character*16 IPCC_scenario                         !! (wh)
    !                    ! Name of IPCC scenario
    !
    ! Define the atmexp namelist
    !

    !
    ! Disclaimer: The namelist items, nhstpr, fhstpr1-fhstpr6, fhstwrtpr1-fwrtpr6,
    ! ideal_phys, trace_gas, bndtvg, sulfdata, scenario_ghg, scenario_so4,
    ! scenario_scon, rampYear_ghg, rampYear_so4, and rampYear_scon
    ! are considered unsuported features. The code may not even run with
    ! these options and has NOT been verified to create correct science.
    ! As such these options should only be used at the users discression.
    !
    namelist /atmexp/ ctitle  ,ncdata  ,bndtvs  ,bndtvo  , bndtvg , &
        bndtvaer, & !! sxj-
        rest_pfile,mss_wpass,nsrest  ,mss_irt , archive_dir, &
        nrevsn  ,nhstpr  ,ndens   ,nhtfrq  , &
        nrefrq  ,mfilt   ,absems_data , &
        fincl1  ,fincl2  ,fincl3  ,fincl4  ,fincl5  , &
        fincl6  ,fexcl1  ,fexcl2  ,fexcl3  ,fexcl4  , &
        fexcl5  ,fexcl6  ,hfilename_spec, &
        fhstpr1 ,fhstpr2 ,fhstpr3 ,fhstpr4 ,fhstpr5 ,fhstpr6 , &
        fwrtpr1 ,fwrtpr2 ,fwrtpr3, fwrtpr4 ,fwrtpr5 ,fwrtpr6 , &
        calendar, dtime, nelapse, nestep, start_ymd, start_tod, dtdy, & !!(wh)
        stop_ymd, stop_tod, ref_ymd, ref_tod, perpetual_run,  &
        perpetual_ymd,   precc_thresh, precl_thresh, &
        eps     ,dfs0    ,                  iradsw  , &
        iradlw  ,iradae  ,itsst   ,nlvdry  ,sstcyc  , &
        ozncyc  , &
        pertlim , &
        caseid  ,adiabatic,flxave , &
        trace_gas,trace_test1   ,   &
        trace_test2,trace_test3   ,readtrace, &
        co2vmr  ,ch4vmr  ,n2ovmr  ,f11vmr  ,f12vmr  , &
        obliq   ,eccen   ,mvelp   ,iyear_AD,scon    , &
        inithist, tauvis,   linebuf, &
        nusr_adv, nusr_nad, aqua_planet, &
        indirect, sulfdata, &
        scenario_ghg, scenario_so4, scenario_scon, &
        rampYear_ghg, rampYear_so4, rampYear_scon, empty_htapes, &
        print_step_cost, avgflag_pertape,prognostic_icesnow, &
        reset_csim_iceprops, &
        ideal_phys, convection_scheme, & ! physical package
        IPCC_scenario

    ! WAN Hui added the following namelist variables:
    ! * dfs0                  - 2004/04/14
    ! * convection_scheme     - 2004/12/17


    !
    !------------------------------Externals--------------------------------
    !
    character(len=8), external :: upcase ! Uppercase 8-character variable
    !
    !-----------------------------------------------------------------------
    !
    ! Preset convection scheme
    !
    convection_scheme = "Zhang-Hack"
    !
    !  Preset stratiform scheme
    !
    ! Preset scenario variables and ramping year
    !
    scenario_ghg  = 'FIXED'
    scenario_so4  = 'FIXED'
    scenario_scon = 'FIXED'
    rampYear_ghg  = 0
    rampYear_so4  = 0
    rampYear_scon = 0
    !
    ! Preset sulfate aerosol related variables
    !
    IPCC_scenario = ' '
    sulfdata      = ' '
    indirect      = .false.
    !
    ! Set anncyc true, no longer in namelist
    !
    anncyc = .true.
    if (masterproc) then
        !
        ! Read in the atmexp namelist from standard input
        !
        read(5,atmexp,iostat=ierr)

        if (ierr /= 0) then
            write(6,*)'PARSE_NAMELIST: Namelist read returns ',ierr
            !!         call endrun                        !!(wh 2003.11.18)
        end if
        !
        ! Check CASE namelist variable
        !
        if (caseid==' ') then
            write(6,*)'PARSE_NAMELIST: Namelist variable CASEID must be set'
            call endrun
        end if

        lastchar = len(caseid)
        if (caseid(lastchar:lastchar) /= ' ') then
            write(6,*)'PARSE_NAMELIST: CASEID must not exceed ', len(caseid)-1, &
                ' characters'
            call endrun
        end if
        icecyc = sstcyc    ! ice-cycling is tied to the sst-dataset

        !
        ! Data ice-model can not use prognostic snow-depth or reset the ice properties
        !
        if ( icemodel_is('data') )then
            if ( .not. prognostic_icesnow ) &
                write(6,*) 'Warning: prognostic_icesnow for data-ice-model is always false'
            prognostic_icesnow = .false.
            if ( .not. reset_csim_iceprops ) &
                write(6,*) 'Warning: reset_csim_iceprops for data-ice-model is always false'
            reset_csim_iceprops = .false.
        end if

    end if
    !
    ! Line buffer stdout if requested
    !
    if (linebuf) then
        !        call flush(6)
        call linebuf_stdout ()
    end if
    !
    ! Precipitation thresholds (check range and convert to mm/hr)
    !
    if ( precc_thresh < 0.0_r8 ) then
        write(6,*)'PARSE_NAMELIST: PRECC threshold needs to be >= 0.0.'
        call endrun
    endif
    if ( precc_thresh > 9.99_r8 ) then
        write(6,*)'PARSE_NAMELIST: PRECC threshold needs to be <= 9.99 mm/hr.'
        call endrun
    endif
    if ( precl_thresh < 0.0_r8 ) then
        write(6,*)'PARSE_NAMELIST: PRECL threshold needs to be >= 0.0.'
        call endrun
    endif
    if ( precl_thresh > 9.99_r8 ) then
        write(6,*)'PARSE_NAMELIST: PRECL threshold needs to be <= 9.99 mm/hr.'
        call endrun
    endif
    precc_thresh = precc_thresh/(1000.0*3600.0) ! convert to m/sec
    precl_thresh = precl_thresh/(1000.0*3600.0) ! convert to m/sec

    call distnl ( scenario_ghg , rampYear_ghg , scenario_so4 , &
        rampYear_so4 , scenario_scon, rampYear_scon, &
        sulfdata,      IPCC_scenario )   !!(wh)


    ! Communicate to time manager (there should be a method for this).
    tm_aqua_planet = aqua_planet

    !
    ! Set continuation run flags
    !
    if (nsrest>0) then
        nlres  = .true.
    endif
    if (nsrest==2) then
        write(6,*)'PARSE_NAMELIST: The regeneration option is no longer available'
        call endrun
    end if
    if (nsrest==3) then
        nlhst  = .true.
        lbrnch = .true.
    endif

# 603 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/parse_namelist.F90"
    !++mv

    if (convection_scheme /= 'Zhang-Hack' .and. convection_scheme /= 'Tiedtke'.and.convection_scheme/='Zhang_Neale') then
        write(6, "('Error: parse_namelist: input namelist ')", advance="no")
        write(6, "('""convection_scheme"" must be set to either Zhang-Hack or Tiedtke')")
        call endrun
    end if
    !
    ! Determine ramping logic
    !
    if (scenario_ghg == 'FIXED') then
        doRamp_ghg = .false.
    else if (scenario_ghg == 'RAMPED') then
        doRamp_ghg = .true.
    else if (scenario_ghg == 'CMIP5') then
        doCmip5_ghg = .true.
        doRamp_ghg  = .false.
    else
        write(6,*)' PARSE_NAMELIST: input namelist SCENARIO_GHG must be set to either FIXED or RAMPED'
        call endrun
    endif
    ! added by WAN Hui
    if (scenario_so4 == 'FIXED') then
        doIPCC_so4 = .false.
        doRamp_so4 = .false.
    else if (scenario_so4 == 'RAMPED') then
        doIPCC_so4 = .false.
        doRamp_so4 = .true.
        if (sulfdata == ' ') then
            write(6,*)'PARSE_NAMELIST: SULFDATA must be specified for SCENARIO_SO4 set to RAMPED'
            call endrun
        endif
    else if (scenario_so4 == 'IPCC') then
        doIPCC_so4 = .true.
        doRamp_so4 = .false.
        if (sulfdata == ' ') then
            write(6,*)'PARSE_NAMELIST: SULFDATA must be specified for SCENARIO_SO4 set to IPCC'
            call endrun
        endif
        klen=len_trim(sulfdata)
        if ( sulfdata(klen:klen) /= '/' ) then
            write(6,*) 'PARSE_NAMELIST: SCENARIO_SO4 has been set to IPCC thus SULFDATA should &
                &be the path of input sulfate dataset . Do not forget the trailing ''/'' !'
            call endrun
        endif
        if ( IPCC_scenario == ' ') then
            write(6,*)'PARSE_NAMELIST: IPCC_scenario must be specified for SCENARIO_SO4 set to IPCC'
            call endrun
        endif
    else
        write(6,*)' PARSE_NAMELIST: input namelist SCENARIO_SO4 must be set to either', &
            ' FIXED , RAMPED or IPCC'
        call endrun
    endif

   if (scenario_scon == 'FIXED') then
      doRamp_scon = .false.
   else if (scenario_scon == 'RAMPED') then
      doRamp_scon = .true.
   else if (scenario_scon == 'CMIP5') then
      doCmip5_scon = .true.
      doRamp_scon  = .false.
   else
      write(6,*)' PARSE_NAMELIST: input namelist SCENARIO_SCON must be set to either FIXED or RAMPED'
      call endrun
   endif
    !
    ! Initialize namelist related ghg info
    !
    if (doRamp_ghg) then
        call rampnl_ghg( rampYear_ghg )
        if (masterproc) write(6,*) 'co2,nh4,n2o,cfc11,cfc12 volume mixing ratios set by ghg ramp code'
    else
        if (masterproc) then
            write(6,*) 'global co2 volume mixing ratio = ',co2vmr
            write(6,*) 'global ch4 volume mixing ratio = ',ch4vmr
            write(6,*) 'global n2o volume mixing ratio = ',n2ovmr
            write(6,*) 'global f11 volume mixing ratio = ',f11vmr
            write(6,*) 'global f12 volume mixing ratio = ',f12vmr
        end if
    endif
    !
    ! Initialize namelist related so4 info
    !
    if (doRamp_so4) then
        call rampnl_so4( rampYear_so4 )
        call so4bndnl( sulfdata )
    endif

    if (doIPCC_so4) then                                 !!
        call so4bndnl_IPCC( sulfdata, IPCC_scenario )     !!(wh)
    endif                                                !!
    !
    ! Initialize namelist related scon info
    !
    if (doRamp_scon) then
        call rampnl_scon( rampYear_scon )
        if (masterproc) write(6,*)'scon set by ramp code'
    else
        if (masterproc) write(6,*)'scon set to fixed value of ',scon
    endif
    if (doCmip5_scon) then
        call cmip5nl_scon( rampYear_scon )
        if (masterproc) write(6,*)'scon set by cmip5 code'
    else
        if (masterproc) write(6,*)'scon set to fixed value of ',scon
    endif
    !
    ! Auxiliary history files:
    ! Store input auxf values in array aux (from common block /comhst/).
    !
    ! If generate an initial conditions history file as an auxillary tape:
    !
    ctemp = upcase(inithist)
    inithist = trim(ctemp)
    if (inithist /= 'MONTHLY' .and. inithist /= 'YEARLY') then
        inithist = 'NONE'
    endif
    !
    ! Ensure that monthly averages have not been specified for aux. tapes
    !
    do t=2,ptapes
        if (nhtfrq(t) == 0) then
            write(6,*)'PARSE_NAMELIST: Only the primary history file may be monthly averaged'
            call endrun
        end if
    end do
    !
    ! History file write up times
    ! Convert write freq. of hist files from hours to timesteps if necessary.
    !
    do t=1,ptapes
        if (nhtfrq(t) < 0) then
            nhtfrq(t) = nint((-nhtfrq(t)*3600.)/dtime)
        end if
    end do
    !
    ! Initialize the filename specifier if not already set
    ! This is the format for the history filenames:
    ! %c= caseid, %t=tape no., %y=year, %m=month, %d=day, %s=second, %%=%
    ! See the filenames module for more information
    !
    do t = 1, ptapes
        if ( len_trim(hfilename_spec(t)) == 0 )then
            if ( nhtfrq(t) == 0 )then
                hfilename_spec(t) = '%c.gamil.h%t.%y-%m.nc'        ! Monthly files
            else
                hfilename_spec(t) = '%c.gamil.h%t.%y-%m-%d-%s.nc'
            end if
        end if
        if ( masterproc ) then
            write(6,*) 'Filename specifier for tape ', t, ' = ', &
                trim(hfilename_spec(t))
        end if
    end do
    !
    ! Only one time sample allowed per monthly average file
    !
    if (nhtfrq(1) == 0) mfilt(1) = 1
    !
    ! Check validity of per-tape averaging flag
    !
    do t=1,ptapes
        if (avgflag_pertape(t) /= ' ') then
            if (avgflag_pertape(t) == 'A' .or. avgflag_pertape(t) == 'I' .or. &
                avgflag_pertape(t) == 'X' .or. avgflag_pertape(t) == 'M') then
                write(6,*)'Unless overridden by namelist input on a per-field basis (FINCL),'
                write(6,*)'All fields on history file ',t,' will have averaging flag ',avgflag_pertape(t)
            else
                write(6,*)'Invalid per-tape averaging flag specified:', avgflag_pertape(t)
                call endrun ()
            end if
        end if
    end do
    !
    ! Convert iradsw and iradlw from hours to timesteps if necessary
    !
    if (iradsw < 0) iradsw = nint((-iradsw*3600.)/dtime)
    if (iradlw < 0) iradlw = nint((-iradlw*3600.)/dtime)
    !
    ! Convert iradae from hours to timesteps if necessary and check that
    ! iradae must be an even multiple of iradlw
    !
    if (iradae < 0) iradae = nint((-iradae*3600.)/dtime)
    if (mod(iradae,iradlw)/=0) then
        write(6,*)'PARSE_NAMELIST:iradae must be an even multiple of iradlw.'
        write(6,*)'     iradae = ',iradae,', iradlw = ',iradlw
        call endrun
    end if
    !
    ! Do absorptivities/emissivities have to go on a restart dataset?
    !
    if (nhtfrq(1) /= 0) then
        if (masterproc .and. mod(nhtfrq(1),iradae)/=0) then
            write(6,*)'PARSE_NAMELIST: *** NOTE: Extra overhead invoked putting',  &
                ' a/e numbers on restart dataset. ***   ',         &
                ' To avoid, make mod(nhtfrq,iradae) = 0'
        end if
    else
        ntspdy = nint(86400./dtime) ! no. timesteps per day
        if (masterproc) then
            if (mod(ntspdy,iradae) /= 0 .or. iradae > ntspdy) then
                write(6,*)'PARSE_NAMELIST: *** NOTE: Extra overhead invoked',  &
                    ' putting a/e numbers on restart dataset. ***'
                write(6,*)' To avoid, make mod(timesteps per day,iradae)= 0'
            end if
        end if
    end if
    !
    !! Number of levels to apply Courant limiter
    !!
    !!   if (kmxhdc >= plev .or. kmxhdc < 0) then
    !!      write(6,*)'PARSE_NAMELIST: KMXHDC must be between 0 and plev-1'
    !!      call endrun
    !!   end if
    !
    ! Build MSS pathname for restart file for branch run.
    ! Note that full (absolute) pathname must be input as nrevsn.
    !
    if (lbrnch .and. (nrevsn(1:1) /= '/') ) then
        write(6,*)'PARSE_NAMELIST: for BRANCH run, NREVSN must be a full pathname.'
        call endrun
    endif
    !
    ! Restart files write frequency (on or off)
    !



    if (nrefrq /= 0) then
        if ((nrefrq /= 1)) then
            write(6,*) 'PARSE_NAMELIST: the value of NREFRQ must be 1 or 0'
            call endrun
        endif
    end if

    !
    ! Initialize file paths module
    !
    call init_filepaths( archivedirname=archive_dir )
    !
    ! If branch set restart filepath to path given on namelist
    !
    if ( lbrnch ) call set_restart_filepath( nrevsn )
    !
    ! Print atmexp input variables to standard output
    !
    if (masterproc) then
        write(6,*)'PARSE_NAMELIST:rest_pfile= ',rest_pfile
        write(6,*)' ------------------------------------------'
        write(6,*)'     *** INPUT VARIABLES (ATMEXP) ***'
        write(6,*)' ------------------------------------------'
        if (nlres) then
            write(6,*) '  Continuation of an earlier run'
        else
            write(6,*) '         Initial run'
        end if
        write(6,*) ' ********** CASE = ',trim(caseid),' **********'
        write(6,'(1x,a)') ctitle
        if (len_trim(ncdata) > 0) then
            write(6,*) 'Initial dataset is: ',trim(ncdata)
        end if
        write(6,*) ' History-file archive directory = ', trim(get_archivedir('hist'))
        write(6,*) ' Restart-file archive directory = ', trim(get_archivedir('rest'))
        write(6,*) ' Initial-file archive directory = ', trim(get_archivedir('init'))

        write(6,*)'Time-variant boundary dataset (sst) is: ', trim(bndtvs)

        write(6,*)'Time-variant boundary dataset (ozone) is: ', trim(bndtvo)
        write(6,*)'Time-invariant (absorption/emissivity) factor dataset is: ', trim(absems_data)
        if ( trace_gas ) then
            write(6,*)'Time-variant boundary dataset (greenhouse loss rates) is: ', trim(bndtvg)
        end if
        !
        ! Restart files info
        !
        if (nrefrq == 1) then
            write(6,*)'PARSE_NAMELIST3:rest_pfile=',rest_pfile
            write(6,*)'Restart pointer file is: ',trim(rest_pfile)
        else if (nrefrq==0) then
            write(6,*) 'NO RESTART DATASET will be written'
        endif



        !
        ! Write password
        !
        if (mss_wpass /='        ') then
            write(6,*)'Write passwd for output tapes (MSS_WPASS) is ', mss_wpass
        end if
        !
        ! Type of run
        !
        write(6,*)'Restart flag (NSREST) 0=no,1=yes,3=branch ',nsrest
    end if
    !
    ! Print retention period for mass store
    !
    if (mss_irt > 0) then
        if (mss_irt > 4096) then
            mss_irt = 4096
        end if
        if (masterproc) then
            write(6,*) 'Retention time for output files = ',mss_irt,' days'
        end if
    else
        if (masterproc) write(6,*) 'Output files will NOT be disposed to Mass Store'
    end if
    !
    ! History file info
    !
    if (masterproc) then
        if (inithist == 'MONTHLY' ) then
            write(6,*)'Initial conditions history files will be written monthly.'
        else if (inithist == 'YEARLY' ) then
            write(6,*)'Initial conditions history files will be written yearly.'
        else
            write(6,*)'Initial conditions history files will not be created'
        end if
    end if
    !
    ! Write physics variables from namelist atmexp to std. output
    !
    if (masterproc) then




        !!      if ( dif4 == inf ) then
        !!         write(6,*) 'PARSE_NAMELIST:  Error, dif4 was not in your namelist. ', &
        !!                    'dif4 MUST be set on the namelist for this resolution.'
        !!         call endrun
        !!      end if
        !!
        !!      write(6,9108) eps,dif2,dif4,kmxhdc,nlvdry
        write(6,9110) iradsw,iradlw,iradae,itsst

        !!9108 format(' Time filter coefficient (EPS)                 ',f10.3,/,&
        !!            ' DEL2 Horizontal diffusion coefficient (DIF2)  ',e10.3/, &
        !!            ' DEL4 Horizontal diffusion coefficient (DIF4)  ',e10.3/, &
        !!            ' Number of levels Courant limiter applied      ',i10/,   &
        !!            ' Lowest level for dry adiabatic adjust (NLVDRY)',i10)

9110    format(' Frequency of Shortwave Radiation calc. (IRADSW)     ',i5/, &
            ' Frequency of Longwave Radiation calc. (IRADLW)      ',i5/,  &
            ' Frequency of Absorptivity/Emissivity calc. (IRADAE) ',i5/, &
            ' Frequency of SST Initialization calc. (ITSST)       ',i5)

        if (sstcyc) then
            write(6,*)'SST dataset will be reused for each model year'
        else
            write(6,*)'SST dataset will not be cycled'
        end if


        if ( icemodel_is('csim') .and. reset_csim_iceprops) then
            write(6,*)'CSIM ICE properties being reset to a new base state'
        end if
        if (prognostic_icesnow) then
            write(6,*)'Snow will accumulate to a maximum over sea-ice'
        else
            write(6,*)'Snow over sea-ice will be set to a climatology'
        end if


        if (icecyc) then
            write(6,*)'ICE dataset will be reused for each model year'
        else
            write(6,*)'ICE dataset will not be cycled'
        end if

        if (ozncyc) then
            write(6,*)'OZONE dataset will be reused for each model year'
        else
            write(6,*)'OZONE dataset will not be cycled'
        end if

        write (6,*)'Output files will be disposed ASYNCHRONOUSLY'

        !!      if (divdampn > 0.) then
        !!         write(6,*) 'Divergence damper invoked for days 0. to ',divdampn,' of this case'
        !!      elseif (divdampn < 0.) then
        !!         write(6,*) 'PARSE_NAMELIST:  Error, divdampn must be a positive number'
        !!         call endrun
        !!      else
        !!         write(6,*) 'divergence damper NOT invoked'
        !!      endif

        if ( (adiabatic .and. ideal_phys) .or. (adiabatic .and. aqua_planet) .or. &
            (ideal_phys .and. aqua_planet) ) &
            then
            write(6,*) 'PARSE_NAMELIST:  Error, only one of the namelist variables "adiabatic", ', &
                '"ideal_phys", or "aqua_planet" can be set to ".true."'
            call endrun
        end if

        if (adiabatic)   write(6,*) 'Model will run ADIABATICALLY (i.e. no physics)'
        if (ideal_phys)  write(6,*) 'Run ONLY the "idealized" dynamical core of the ', &
            'model  (dynamics + Held&Suarez-specified physics)'
        if (aqua_planet) write(6,*) 'Run model in "AQUA_PLANET" mode'
        !
        ! Write tracer initialization info.
        !
        if (pcnst > 1) then
            if (nusr_adv > 0) write(6,*) 'number of user advected tracers= ',nusr_adv
            if (nusr_nad > 0) write(6,*) 'number of user non-advected tracers= ',nusr_nad
            if (trace_gas) write(6,*) 'Implementing greenhouse gas code'
            if (trace_test1) write(6,*)'Implementing test tracer code with radon '
            if (trace_test2) write(6,*)'Implementing test tracer code with radon and conserved unit tracer'
            if (trace_test3) write(6,*)'Implementing test tracer code ', &
                'with radon, conserved unit tracer and ozone-like tracer'
            if (readtrace) then
                write(6,*) 'All tracers will be initialized from initial conditions file'
            else
                if (trace_gas) then
                    write(6,*)'Greenhouse gases will be initialized in routine CHEMMIX'
                endif
                if (trace_test1 .or. trace_test2 .or. trace_test3) then
                    write(6,*)'Test tracers will be initialized in routine INITESTTR'
                endif
                if (nusr_adv > 0) then
                    write(6,*)'All advected user tracers will be initialized to zero'
                endif
                if (nusr_nad > 0) then
                    write(6,*)'All non-advected user tracers will be initialized to zero'
                endif
            endif
        endif
    end if







    if (masterproc) then
        write(6,*) 'Visible optical depth (tauvis) = ',tauvis

# 1068 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/parse_namelist.F90"
        call shr_orb_print( iyear_AD, eccen, obliq, mvelp )
        write(6,*)' ------------------------------------------'

    end if

    return
end subroutine parse_namelist

!=======================================================================

character*(*) function upcase(lstring)
    !-----------------------------------------------------------------------
    !
    ! Convert character string lstring to upper case
    !
    !---------------------------Code history--------------------------------
    !
    ! Original version:  L. Bath
    ! Standardized:      L. Bath, Jun 1992
    !                    L. Buja, Feb 1996
    !
    !-----------------------------------------------------------------------
    !
    ! $Id: parse_namelist.F90,v 1.22.2.14 2002/09/17 05:36:54 erik Exp $
    ! $Author: erik $
    !
    !-----------------------------------------------------------------------
    implicit none
    !---------------------------Arguments-----------------------------------
    !
    ! Input argument
    !
    character(len=*) :: lstring ! String to convert to upper case
    !
    !---------------------------Local variable------------------------------
    !
    integer i                   ! Index
    character(len=1) :: ctmp    ! Character temporary
    !
    !-----------------------------------------------------------------------
    !
    do i=1,len(upcase)
        upcase(i:i) = ' '
    end do
    !
    do i=1,len_trim(lstring)
        upcase(i:i) = lstring(i:i)
        ctmp = upcase(i:i)
        if (ichar(lstring(i:i))>=97.and.ichar(lstring(i:i))<=122) &
            upcase(i:i) = char(ichar(ctmp) - 32)
    end do
    !
    return
end function upcase

!=======================================================================


subroutine distnl ( scenario_ghg , rampYear_ghg , scenario_so4 , &
    rampYear_so4 , scenario_scon, rampYear_scon, &
    sulfdata ,     IPCC_scenario )                  !!(wh)
    !-----------------------------------------------------------------------
    !
    ! Purpose:
    ! Distribute namelist data all processors.
    !
    ! The cpp  definition provides for the funnelling of all program i/o
    ! through the master processor. Processor 0 either reads restart/history
    ! data from the disk and distributes it to all processors, or collects
    ! data from all processors and writes it to disk.
    !
    !---------------------------Code history-------------------------------
    !
    ! Original version:  CCM2
    ! Standardized:      J. Rosinski, Oct 1995
    !                    J. Truesdale, Feb. 1996
    !
    !-----------------------------------------------------------------------
    !
    ! $Id: parse_namelist.F90,v 1.22.2.14 2002/09/17 05:36:54 erik Exp $
    ! $Author: erik $
    !
    !-----------------------------------------------------------------------
    use shr_kind_mod, only: r8 => shr_kind_r8
    use pmgrid
    use pspect
    use mpishorthand
    use history
    use constituents, only: ch4vmr, n2ovmr, f11vmr, f12vmr, co2vmr
    use tracers, only: nusr_adv, nusr_nad
    use moistconvection, only: convection_scheme                     !!(wh 2004.12.17)
    use comhd,   only: dfs0                                          !!(wh 2004.04.14)
    !! use time_manager, only: calendar, dtime, nestep, nelapse, &
    use time_manager, only: calendar, dtime, nestep, nelapse, dtdy,& !!(wh 2004.04.14)
        start_ymd, start_tod, stop_ymd, stop_tod, ref_ymd, ref_tod, &
        perpetual_run, perpetual_ymd

    use ice_dh, only: reset_csim_iceprops,prognostic_icesnow

    use filenames, only: nrevsn, ncdata, bndtvs, bndtvo, absems_data, bndtvg, &
        mss_wpass, mss_irt, caseid,bndtvaer               !! sxj add bndtvaer



    !-----------------------------------------------------------------------
    implicit none


# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/comadj.h" 1
!
! $Id: comadj.h,v 1.6 2001/08/10 22:07:55 boville Exp $
! $Author: boville $
!
!
! Convective adjustment
!
      common/comadj/ nlvdry
!
      integer nlvdry        ! Number of levels to apply dry adjustment
!

 
# 1176 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/parse_namelist.F90" 2

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
# 1177 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/parse_namelist.F90" 2
    !!#include <comhd.h>

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

# 1179 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/parse_namelist.F90" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/comtfc.h" 1
!----------------------------------------------------------------------- 
! 
! Purpose: time filter coefficient
! 
!-----------------------------------------------------------------------
      common /comtfc/ eps

      real(r8) eps             ! Time filter coefficient
 
# 1180 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/parse_namelist.F90" 2
    !---------------------------Arguments-----------------------------------
    !
    integer, intent(in) :: &
        rampYear_ghg,       &! year that ramped ghg gases are fixed at
        rampYear_so4,       &! year that ramped sulfate is fixed at
        rampYear_scon        ! year that ramped scon is fixed at
    character*16, intent(in) :: &
        scenario_ghg,       &! ghg volume mixing ratios scenario
        scenario_so4,       &! sulfate scenario
        scenario_scon,      &! scon scenario
        IPCC_scenario
    character(len=*), intent(in) :: sulfdata
    !
    !-----------------------------------------------------------------------
    !
    call mpibcast (calendar,   32,mpichar,0,mpicom)
    call mpibcast (dtime,       1,mpiint,0,mpicom)
    call mpibcast (dtdy,        1,mpir8, 0,mpicom)   !!(wh 2004.04.14)
    call mpibcast (nestep,      1,mpiint,0,mpicom)
    call mpibcast (nelapse,     1,mpiint,0,mpicom)
    call mpibcast (start_ymd,   1,mpiint,0,mpicom)
    call mpibcast (start_tod,   1,mpiint,0,mpicom)
    call mpibcast (stop_ymd,    1,mpiint,0,mpicom)
    call mpibcast (stop_tod,    1,mpiint,0,mpicom)
    call mpibcast (ref_ymd,     1,mpiint,0,mpicom)
    call mpibcast (ref_tod,     1,mpiint,0,mpicom)
    call mpibcast (perpetual_run, 1,mpilog,0,mpicom)
    call mpibcast (perpetual_ymd, 1,mpiint,0,mpicom)

    !! (wanhui 2003.07.21)
    call mpibcast (nhstpr  ,ptapes,mpiint,0,mpicom)
    call mpibcast (ndens   ,ptapes,mpiint,0,mpicom)
    call mpibcast (nhtfrq  ,ptapes,mpiint,0,mpicom)
    call mpibcast (mfilt   ,ptapes,mpiint,0,mpicom)
    call mpibcast (nusr_adv,1,mpiint,0,mpicom)
    call mpibcast (nusr_nad,1,mpiint,0,mpicom)
    call mpibcast (nsrest  ,1,mpiint,0,mpicom)
    call mpibcast (mss_irt ,1,mpiint,0,mpicom)
    call mpibcast (nrefrq  ,1,mpiint,0,mpicom)
    !! call mpibcast (kmxhdc  ,1,mpiint,0,mpicom)
    call mpibcast (iradsw  ,1,mpiint,0,mpicom)
    call mpibcast (iradlw  ,1,mpiint,0,mpicom)
    call mpibcast (iradae  ,1,mpiint,0,mpicom)
    call mpibcast (itsst   ,1,mpiint,0,mpicom)
    call mpibcast (nlvdry  ,1,mpiint,0,mpicom)

    call mpibcast (reset_csim_iceprops,1,mpilog,0,mpicom)
    call mpibcast (prognostic_icesnow,1,mpilog,0,mpicom)

!!! f-v dynamics specific
    !!   call mpibcast (nsplit  ,1,mpiint,0,mpicom)
    !!   call mpibcast (iord    ,1,mpiint,0,mpicom)
    !!   call mpibcast (jord    ,1,mpiint,0,mpicom)
    !!   call mpibcast (kord    ,1,mpiint,0,mpicom)
    !!   call mpibcast (use_eta ,1,mpilog,0,mpicom)

    !!   call mpibcast (divdampn,1,mpir8,0,mpicom)
    call mpibcast (co2vmr  ,1,mpir8,0,mpicom)
    call mpibcast (ch4vmr  ,1,mpir8,0,mpicom)
    call mpibcast (n2ovmr  ,1,mpir8,0,mpicom)
    call mpibcast (f11vmr  ,1,mpir8,0,mpicom)
    call mpibcast (f12vmr  ,1,mpir8,0,mpicom)
    call mpibcast (tauvis  ,1,mpir8,0,mpicom)
    call mpibcast (eps     ,1,mpir8,0,mpicom)
    call mpibcast (dfs0    ,1,mpir8,0,mpicom)   !!(wh 2004.04.14)
    !!   call mpibcast (dif2    ,1,mpir8,0,mpicom)
    !!   call mpibcast (dif4    ,1,mpir8,0,mpicom)
    !!  (2003.07.21)

    call mpibcast (precc_thresh,1,mpir8,0,mpicom)
    call mpibcast (precl_thresh,1,mpir8,0,mpicom)

    call mpibcast (flxave      ,1,mpilog,0,mpicom)
    call mpibcast (adiabatic   ,1,mpilog,0,mpicom)
    call mpibcast (trace_gas   ,1,mpilog,0,mpicom)
    call mpibcast (trace_test1 ,1,mpilog,0,mpicom)
    call mpibcast (trace_test2 ,1,mpilog,0,mpicom)
    call mpibcast (trace_test3 ,1,mpilog,0,mpicom)
    call mpibcast (readtrace   ,1,mpilog,0,mpicom)
    call mpibcast (sstcyc      ,1,mpilog,0,mpicom)
    call mpibcast (icecyc      ,1,mpilog,0,mpicom)
    call mpibcast (ozncyc      ,1,mpilog,0,mpicom)
    call mpibcast (ideal_phys  ,1,mpilog,0,mpicom)
    call mpibcast (aqua_planet ,1,mpilog,0,mpicom)
    call mpibcast (empty_htapes,1,mpilog,0,mpicom)
    call mpibcast (print_step_cost,1,mpilog,0,mpicom)

    call mpibcast (caseid  ,len(caseid) ,mpichar,0,mpicom)
    call mpibcast (avgflag_pertape, ptapes, mpichar,0,mpicom)
    call mpibcast (ctitle  ,len(ctitle),mpichar,0,mpicom)
    call mpibcast (ncdata  ,len(ncdata) ,mpichar,0,mpicom)
    call mpibcast (bndtvs  ,len(bndtvs) ,mpichar,0,mpicom)
    call mpibcast (bndtvo  ,len(bndtvo) ,mpichar,0,mpicom)
    call mpibcast (bndtvaer  ,len(bndtvaer) ,mpichar,0,mpicom)  !! sxj-
    call mpibcast (absems_data,len(absems_data),mpichar,0,mpicom)
    call mpibcast (bndtvg  ,len(bndtvg),mpichar,0,mpicom)
    call mpibcast (mss_wpass,len(mss_wpass)  ,mpichar,0,mpicom)
    call mpibcast (nrevsn  ,len(nrevsn) ,mpichar,0,mpicom)
    call mpibcast (inithist,len(inithist)  ,mpichar,0,mpicom)
    call mpibcast (fincl   ,10*pflds*ptapes,mpichar,0,mpicom)
    call mpibcast (fexcl   , 8*pflds*ptapes,mpichar,0,mpicom)
    call mpibcast (fhstpr  ,10*pflds*ptapes,mpichar,0,mpicom)
    call mpibcast (fwrtpr  ,10*pflds*ptapes,mpichar,0,mpicom)
    call mpibcast (sulfdata,len(sulfdata),mpichar,0,mpicom)
    !
    ! Orbital stuff
    !
    call mpibcast (scon    ,1  ,mpir8 ,0,mpicom)
    call mpibcast (eccen   ,1  ,mpir8 ,0,mpicom)
    call mpibcast (obliq   ,1  ,mpir8 ,0,mpicom)
    call mpibcast (mvelp   ,1  ,mpir8 ,0,mpicom)
    call mpibcast (iyear_ad,1  ,mpiint,0,mpicom)
    !
    call mpibcast (convection_scheme,16 ,mpichar,0,mpicom)  !!(wh 2004.12.17)
    call mpibcast (IPCC_scenario,16 ,mpichar,0,mpicom)      !!(wh)
    call mpibcast (scenario_ghg ,16 ,mpichar,0,mpicom)
    call mpibcast (scenario_so4 ,16 ,mpichar,0,mpicom)
    call mpibcast (scenario_scon,16 ,mpichar,0,mpicom)
    call mpibcast (rampYear_ghg , 1 ,mpiint, 0,mpicom)
    call mpibcast (rampYear_so4 , 1 ,mpiint, 0,mpicom)
    call mpibcast (rampYear_scon, 1 ,mpiint, 0,mpicom)
    call mpibcast (indirect     , 1 ,mpilog, 0,mpicom)


    return
end subroutine distnl
