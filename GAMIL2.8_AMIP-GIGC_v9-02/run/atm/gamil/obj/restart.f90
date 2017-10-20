# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/restart.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/restart.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/restart.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/restart.F90" 2

!! (wanhui 2003.07.10)
!!------------------------


module restart
!----------------------------------------------------------------------- 
! 
! module to handle reading and writing of the master restart files.
!
!----------------------------------------------------------------------- 
! $Id: restart.F90,v 1.9.2.9 2002/08/28 15:34:17 erik Exp $
!----------------------------------------------------------------------- 
   use shr_kind_mod, only: r8 => shr_kind_r8
   use pmgrid,       only: masterproc, plev, plevp, plond, plat,plevstd,plond,plat
!! use rgrid,        only: nlon, wnummax
   use rgrid,        only: nlon
   use ioFileMod,    only: putfil, getfil, opnfil
   use filenames,    only: get_archivedir


   use mpishorthand, only: mpicom, mpir8, mpiint, mpilog


   implicit none
!
! Public interfaces
!
   public write_restart          ! Write the master restart file out
   public read_restart           ! Read the master restart file in
   public set_restart_filepath   ! Set the full filepath to the master restart file
!
! Everything else is private
!
   private

   integer, parameter :: nlen = 256 ! Length of character strings
   character(len=nlen) :: pname     ! Full restart pathname
!
! Filename specifiers for master restart filename
! (%c = caseid, $y = year, $m = month, $d = day, $s = seconds in day, %t = number)
!
   character(len=256) :: rfilename_spec = '%c.gamil.r.%y-%m-%d-%s'
!
! Common blocks
!
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
# 51 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/restart.F90" 2
!-----------------------------------------------------------------------

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
# 53 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/restart.F90" 2
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
 
# 55 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/restart.F90" 2
!-----------------------------------------------------------------------
!!#include <comtfc.h>
!-----------------------------------------------------------------------

CONTAINS

   subroutine write_restart
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Write the primary, secondary, and history buffer regeneration files.
! 
! Method: 
! The cpp  definition provides for the funnelling of all program i/o
! through the master processor. Processor 0 either reads restart/history
! data from the disk and distributes it to all processors, or collects
! data from all processors and writes it to disk.
! 
! Author: 
! 
!----------------------------------------------------------------------- 
      use history,          only: write_restart_history
      use restart_physics,  only: write_restart_physics
      use restart_dynamics, only: write_restart_dynamics
      use time_manager,     only: timemgr_write_restart
      use filenames,        only: caseid, mss_wpass, mss_irt, get_archivedir, &
                                  interpret_filename_spec
!
! Local workspace
!
      integer ioerr             ! write error status
      character(len=256) fname  ! Restart filename
!-----------------------------------------------------------------------
! Write the primary restart datasets
!-----------------------------------------------------------------------
!



!
! Everything inside the following "if" is from the obsolete RGNFLS
!
      if (masterproc) then
!     
! Open master restart file.
!
         fname = interpret_filename_spec( rfilename_spec )
         call opnfil (fname, nrg, 'u')
!
!-----------------------------------------------------------------------
! Write the master restart dataset
!-----------------------------------------------------------------------

         call timemgr_write_restart(nrg)

!!       write (nrg, iostat=ioerr) eps, caseid, hyai, hybi, hyam,  &
!!                                  hybm, aeres
         write (nrg, iostat=ioerr)      caseid, hyai, hybi, aeres
         if (ioerr /= 0 ) then
            write (6,*) 'WRITE ioerror ',ioerr,' on i/o unit = ',nrg
            call endrun
         end if
!
! Reduced grid stuff
!
!!       write (nrg, iostat=ioerr) nlon, wnummax
         write (nrg, iostat=ioerr) nlon
         if (ioerr /= 0 ) then
            write (6,*) 'WRITE ioerror ',ioerr,' on i/o unit = ',nrg
            call endrun
         end if

      end if
!
!-----------------------------------------------------------------------
! Dynamics, physics, History
!-----------------------------------------------------------------------
!
      call write_restart_dynamics (nrg)
      call write_restart_physics (nrg, nrg2)
      call write_restart_history (nrg, luhrest)

      if (masterproc) then
         close(nrg)
         pname = trim(get_archivedir( 'rest' )) // fname
         call putfil (fname, pname, mss_wpass, mss_irt, (.not. nlend) )
         call write_rest_pfile
      end if

      return
   end subroutine write_restart

!#######################################################################

   subroutine read_restart
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Acquire and position the restart, master, primary and secondary
! datasets for a continuation run
! 
! Method: 
! 
! Author: 
! 
!-----------------------------------------------------------------------
      use phys_grid,        only: phys_grid_init
      use history,          only: read_restart_history
      use filenames,        only: caseid
      use restart_physics,  only: read_restart_physics
      use restart_dynamics, only: read_restart_dynamics
      use time_manager,     only: timemgr_read_restart, timemgr_restart
!-----------------------------------------------------------------------
!
! Local workspace
!
      integer i                     ! Indices
      integer ioerr                 ! read error status
      integer decomp                ! decomposition type

      character*256 locfn           ! Local filename
      character*33  tcase           ! Read in previous case name
!
!-----------------------------------------------------------------------
! Determine full restart pathname
!------------------------------------------------------------------------
!
! Test for existence of a restart pointer dataset (nsrest=1 only).
! If restart pinter dataset exists, obtain it and read it.
!
      if (masterproc) then
         if (.not.nlhst) then
            call read_rest_pfile
         endif
!
!------------------------------------------------------------------------
! Obtain and read the master restart dataset 
!------------------------------------------------------------------------
!




!
! Obtain master restart dataset
!
         call getfil (pname, locfn)
         call opnfil (locfn, nrg, 'u')
!     
! Read comtim, comtfc, comhst, comhed and history buffer variables 
!     
         call timemgr_read_restart(nrg)

!!       read (nrg, iostat=ioerr) eps, tcase, hyai, hybi, hyam,  &
!!                                hybm, aeres
         read (nrg, iostat=ioerr)      tcase, hyai, hybi, aeres
         if (ioerr /= 0 ) then
            write (6,*) 'READ ioerror ',ioerr,' on i/o unit = ',nrg
            call endrun
         end if

         if (lbrnch .and. tcase == caseid) then
            write(6,*) 'READ_RESTART: Must change case name on branch run'
            write(6,*) 'Prev case = ',tcase,' current case = ',caseid
            call endrun
         end if
!
! Reduced grid stuff
!
!!         read (nrg, iostat=ioerr) nlon, wnummax
           read (nrg, iostat=ioerr) nlon
         if (ioerr /= 0 ) then
            write (6,*) 'READ ioerror ',ioerr,' on i/o unit = ',nrg
            call endrun
         end if

!!       write(6,*) 'MASTER RESTART DATASET READ. EPS= ',eps
         write(6,*) 'Files for restart:'

      endif  ! end of if-masterproc


!!    call mpibcast (eps    ,1        ,mpir8  ,0,mpicom)
      call mpibcast (hyai   ,plev+1   ,mpir8  ,0,mpicom) 
      call mpibcast (hybi   ,plev+1   ,mpir8  ,0,mpicom) 
      call mpibcast (hyam   ,plev     ,mpir8  ,0,mpicom)   
      call mpibcast (hybm   ,plev     ,mpir8  ,0,mpicom)   
      call mpibcast (aeres  ,1        ,mpilog ,0,mpicom)
      call mpibcast (nlon,   plat,     mpiint, 0,mpicom)
!!    call mpibcast (wnummax,plat,     mpiint, 0,mpicom)


! Restart the time manager.

      call timemgr_restart()

!-----------------------------------------------------------------------
! Dynamics, physics, history
!-----------------------------------------------------------------------

      call read_restart_dynamics (nrg)
      call initcom ()
      call phys_grid_init
      call read_restart_physics (nrg, nrg2, aeres )
      if (nsrest == 1) then
         call read_restart_history (nrg, luhrest)
      end if
      if (masterproc) close(nrg)      ! Done reading restart info

      return
   end subroutine read_restart

   subroutine write_rest_pfile
!----------------------------------------------------------------------- 
! 
! Purpose: 
!
! Write out the restart pointer file
!
!----------------------------------------------------------------------- 
   use filenames,       only: rest_pfile
   use restart_physics, only: get_abs_restart_filepath
   use history,         only: get_mtapes, get_hist_restart_filepath, &
                              hstwr, get_hfilepath, nfils, mfilt
!-----------------------------------------------------------------------
! for nsds

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
# 282 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/restart.F90" 2
!-----------------------------------------------------------------------
! for aeres

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
# 285 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/restart.F90" 2
!-----------------------------------------------------------------------
   integer t      ! Tape number
   integer mtapes ! Number of tapes that are active

   call opnfil(rest_pfile, nsds, 'f')
   rewind nsds
   write (nsds,'(a)') trim(pname)
   write (nsds,'(//a,a)') '# The following lists the other files needed for restarts', &
                        ' (gamil only reads the first line of this file).'
   write (nsds,'(a,a)') '# The files below refer to the files needed for the master restart file:', &
                       trim(pname)
   if ( aeres )then
      write (nsds,'(a,a)') '# ', trim(get_abs_restart_filepath())
   end if
!
! History files: Need restart history files when not a time-step to write history info
! Need: history files if they are not full
!
   mtapes = get_mtapes( )
   do t=1,mtapes
      if ( .not. hstwr(t) ) then
         write (nsds,'(a,a)') '# ', trim(get_hist_restart_filepath( t ))
      end if
      if ( nfils(t) > 0 .and. nfils(t) < mfilt(t) ) then
         write (nsds,'(a,a)') '# ', trim(get_hfilepath( t ))
      end if
   end do
   close (nsds)
   write(6,*)'(WRITE_REST_PFILE): successfully wrote local restart pointer file ',trim(rest_pfile)
   write(6,'("---------------------------------------")')
   end subroutine write_rest_pfile

   subroutine read_rest_pfile
!----------------------------------------------------------------------- 
! 
! Purpose: 
!
! Read the master restart file from the restart pointer file
!
!----------------------------------------------------------------------- 
   use filenames, only: rest_pfile
!-----------------------------------------------------------------------
! for nsds

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
# 329 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/restart.F90" 2
   character(len=256) :: locfn    ! Local pathname for restart pointer file

   call opnfil (rest_pfile, nsds, 'f', status="old")
   read (nsds,'(a)') pname
   close(nsds)

   end subroutine read_rest_pfile

!-----------------------------------------------------------------------
! BOP
!
! !ROUTINE: set_restart_filepath
!
! !DESCRIPTION: Set the filepath of the specific type of restart file.
!
!-----------------------------------------------------------------------
! !INTERFACE:
subroutine set_restart_filepath( rgpath )
!
! !PARAMETERS:
!
  character(len=*), intent(in)  :: rgpath ! Full pathname to restart file
!
! EOP
!
  if ( trim(rgpath) == '' )then
     write(6,*) 'SET_RESTART_FILEPATH: rgpath sent into subroutine is empty'
     call endrun
  end if
  if ( rgpath(1:1) /= '/' )then
     write(6,*) 'SET_RESTART_FILEPATH: rgpath sent into subroutine is not an absolute pathname'
     call endrun
  end if
  if ( len_trim(rgpath) > nlen )then
     write(6,*) 'SET_RESTART_FILEPATH: rgpath is too long :', rgpath
     call endrun
  end if
  pname = trim(rgpath)
end subroutine set_restart_filepath

end module restart
