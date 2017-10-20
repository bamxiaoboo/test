# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/gamil.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/gamil.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/gamil.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/gamil.F90" 2

!-----------------------------------------------------------------------
!
! Purpose: Entry point for GAMIL
!
!-----------------------------NOTICE------------------------------------
!
! Gridpoint Atmosphere Model of IAP/LASG, version 1.0
!
! Purpose:
!
!   Call initialization, time-stepping, and finalization routines.
!
!-----------------------------------------------------------------------

program gamil

    use shr_kind_mod, only: r8 => SHR_KIND_R8
    use pmgrid
    use dycore
    use history,      only: bldfld, intht
    use units
    use restart,      only: read_restart
    use time_manager, only: get_nstep, is_first_restart_step
    use phys_buffer ! added by SHI Xiangjun and LIU Li
    use ppgrid,       only: pcols, pverp, begchunk, endchunk
    use comsrf,       only: fld_kvh ! added by LIU Li

    use mpishorthand, only: mpicom, nsend, nrecv, nwsend, nwrecv








    use CCPL_interface_mod
    use register_decompositions_mod
    use register_private_variables_mod

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
# 47 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/gamil.F90" 2

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
# 48 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/gamil.F90" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/libs/timing/gpt.inc" 1
!
! Fortran include file to match gpt.h entries.
! Note that the values must match
!
      integer usrsys
      integer wall
      integer pcl_l1dcache_miss
      integer pcl_l2cache_miss
      integer pcl_cycles
      integer pcl_elapsed_cycles
      integer pcl_fp_instr
      integer pcl_loadstore_instr
      integer pcl_instr
      integer pcl_stall

      parameter (usrsys              = 1)
      parameter (wall                = 2)
      parameter (pcl_l1dcache_miss   = 4)
      parameter (pcl_l2cache_miss    = 5)
      parameter (pcl_cycles          = 6)
      parameter (pcl_elapsed_cycles  = 7)
      parameter (pcl_fp_instr        = 8)
      parameter (pcl_loadstore_instr = 9)
      parameter (pcl_instr           = 10)
      parameter (pcl_stall           = 11)
# 49 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/gamil.F90" 2
! added by SHI Xiangjun
! DONG Li: try to merge it with the namelist variable

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/RK_or_MG.h" 1
 
  character(len=2)  RK_or_MG
  parameter (RK_or_MG='MG')

# 52 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/gamil.F90" 2











    character*8 cdate          ! System date
    character*8 ctime          ! System time
    character*13 filenam
    integer iu
    integer nstep           ! Current timestep number.
    integer kvh_idx ! added by LIU Li
    integer i
    !------------------------------Externals--------------------------------




# 83 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/gamil.F90"








    !
    ! Initialize timing library.  2nd arg 0 means disable, 1 means enable
    !
    call t_setoptionf(usrsys, 0)
    call t_initializef

    call t_startf('total')
    call t_startf('initialization')

    !call c_coupler_initialize(mpicom)
    !
    ! Initialize internal/external MPI if appropriate
    !






    !
    ! Initialize  environment if applicable
    !

    call spmdinit

 
    !
    ! Print Model heading and copyright message
    !
    if (masterproc) then
        write(6, *) "----------------------------------------------------------"
        write(6, *) "    Gridpoint Atmosphere Model of IAP/LASG  (GAMIL)       "
        write(6, *) "                    version 2.0                           "
        write(6, *) "----------------------------------------------------------"
    end if
    !
    ! Fetch and print current date and time
    !
    call datetime(cdate, ctime)
    if (masterproc) then
        write(6, *) "DATE ", cdate, " TIME ", ctime
        write(6, *) "----------------------------------------------------------"
        if (dycore_is('EUL')) then
            write(6, *) 'DYCORE is EUL'
        else if (dycore_is('SLD')) then
            write(6, *) 'DYCORE is SLD'
        else if (dycore_is('LR')) then
            write(6, *) 'DYCORE is LR'
        end if
    end if
    !
    ! Set defaults then override with user-specified input
    !
    call preset
    call parse_namelist
    !
    ! Define fortran unit numbers
    !
    nsds    = getunit()
    nrg     = getunit()
    nrg2    = getunit()
    luhrest = getunit()

    if (masterproc) then
        write(6, *)
        write(6, "('=========================================')")
        write(6, "('***Summary of Logical Unit assignments***')")
        write(6, *)
        write(6, "('   Restart pointer unit (nsds)     : ', I2)") nsds
        write(6, "('   Master restart unit (nrg)       : ', I2)") nrg
        write(6, "('   Abs/ems unit for restart (nrg2) : ', I2)") nrg2
        write(6, "('   History restart unit (luhrest)  : ', I2)") luhrest
        write(6, "('=========================================')")
        write(6, *)
    end if
    !
    ! Initialize index values for advected and non-advected tracers
    !
    call initindx
    !
    ! Do appropriate dynamics and history initialization depending on whether initial, restart, or
    ! branch.  On restart run intht need not be called because all the info is on restart dataset.
    !
    select case (nsrest)
    case (0)                ! initial run
        call inital          ! dynamics (mostly) init
        call inti            ! physics init
        call bldfld          ! master field list
        call intht           ! set up history tape contents for this run
    case (1)                ! restart run
        call read_restart    ! read restart file(s)
        call inti            ! physics init
        call bldfld          ! master field list
    case (3)                ! branch run
        call read_restart    ! read restart file(s), minus history info
        call inti            ! physics init
        call bldfld          ! master field list
        call intht           ! set up history tape contents for this run
    case default
        write(6, *)' nsrest=', nsrest, ' must be 0, 1, or 3'
        call endrun
    end select

    call register_decompositions
    call register_static_variables

    !
    ! Initialize external models or datasets depending upon whether coupled
    !
    call initext
    call t_stopf('initialization')
    !
    ! Invoke driving routine for time integration
    !
    if (RK_or_MG == 'MG') then
        call pbuf_allocate('global') ! added by SHI Xiangjun
        if (is_first_restart_step() ) then ! added by LIU Li
            kvh_idx = pbuf_get_fld_idx('KVH')
            do i = begchunk, endchunk
                pbuf(kvh_idx)%fld_ptr(1,1:pcols,1:pverp,i,1) = fld_kvh(1:pcols,1:pverp,i)
            end do
        end if
    end if
    call t_startf('stepon')
    call stepon
    call t_stopf('stepon')
    if (RK_or_MG == 'MG') call pbuf_deallocate('global')  ! added by SHI Xiangjun
    !
    ! End the run cleanly
    !
    call t_stopf('total')
    call t_prf(iam)


    if (.false.) then
        write(0,*)'The following stats are exclusive of initialization/boundary datasets'
        write(0,*)'Number of messages sent by proc ',iam,' is ',nsend
        write(0,*)'Number of messages recv by proc ',iam,' is ',nrecv
    end if


    if (masterproc) then
        nstep = get_nstep()
        write (6,9300) nstep-1,nstep
9300    format (//'Number of completed timesteps:',i6,/,'Time step ',i6, &
            ' partially done to provide convectively adjusted and ', &
            'time filtered values for history tape.')
        write(6,*)'------------------------------------------------------------'
        write(6,*)'******* END OF MODEL RUN *******'
    end if





    call mpibarrier(mpicom)
    call mpifinalize



    iu = getunit ()
    write(filenam,'(a10,i3.3)') 'spmdstats.', iam
    open (unit=iu, file=filenam, form='formatted', status='replace')
    write (iu,*)'iam ',iam,' msgs  sent =',nsend
    write (iu,*)'iam ',iam,' msgs  recvd=',nrecv
    write (iu,*)'iam ',iam,' words sent =',nwsend
    write (iu,*)'iam ',iam,' words recvd=',nwrecv


    stop

end program gamil
