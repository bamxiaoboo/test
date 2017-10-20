# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/restart_physics.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/restart_physics.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/restart_physics.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/restart_physics.F90" 2

!!  (wanhui 2003.07.10)
!!------------------------

module restart_physics

    use shr_kind_mod, only: r8 => shr_kind_r8
    use ppgrid
    use phys_grid,    only: read_chunk_from_field, write_field_from_chunk, get_ncols_p
    use pmgrid,       only: masterproc
    use prognostics,  only: ptimelevels, n3, n3m2
    use buffer
    use radae,        only: abstot_3d, absnxt_3d, emstot_3d, initialize_radbuffer
    use comsrf
    use ioFileMod
    use phys_buffer




    implicit none

    private
    !
    ! Public interfaces
    !
    public write_restart_physics
    public read_restart_physics
    public get_abs_restart_filepath

    !
    ! Private data
    !
    character(len=256) :: pname  ! Full abs-ems restart filepath
    !
    ! Filename specifier for restart abs-ems file
    ! (%c = caseid, $y = year, $m = month, $d = day, $s = seconds in day, %t = tape number)
    !
    character(len=256) :: rafilename_spec = '%c.cam2.ra.%y-%m-%d-%s'   ! abs-ems restart


CONTAINS

    subroutine write_restart_physics (nrg, nrg2)
        use filenames, only: mss_irt, mss_wpass, get_archivedir, interpret_filename_spec
        ! for nlend and aeres

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
# 50 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/restart_physics.F90" 2
        !
        ! Input arguments
        !
        integer :: nrg
        integer :: nrg2
        integer :: kvh_idx
        !
        ! Local workspace
        !
        real(r8) tmpfield(pcols,begchunk:endchunk)
        real(r8) tmpfield3d(pcols,plevmx,begchunk:endchunk)
        real(r8) tmpfield3d2(pcols,pverp,begchunk:endchunk)
        integer i                 ! loop index
        integer n3tmp             ! timestep index
        character(len=256) fname  ! abs-ems restart filename
        integer ioerr             ! I/O status
        integer  :: ncol          ! number of vertical columns
        !
        ! Buffer module variables
        !
        call write_field_from_chunk(nrg,1,1,1,pblht)
        call write_field_from_chunk(nrg,1,1,1,tpert)
        call write_field_from_chunk(nrg,1,pver,1,qrs)
        call write_field_from_chunk(nrg,1,pver,1,qrl)
        call write_field_from_chunk(nrg,1,pcnst+pnats,1,qpert)
        !
        ! cld, qcwat, and tcwat are physics things, but have dynamics time levels
        !
        call write_field_from_chunk(nrg,1,pver,1,cld(1,1,begchunk,n3  ))
        call write_field_from_chunk(nrg,1,pver,1,cld(1,1,begchunk,n3m2))

        call write_field_from_chunk(nrg,1,pver,1,qcwat(1,1,begchunk,n3  ))
        call write_field_from_chunk(nrg,1,pver,1,qcwat(1,1,begchunk,n3m2))

        call write_field_from_chunk(nrg,1,pver,1,tcwat(1,1,begchunk,n3  ))
        call write_field_from_chunk(nrg,1,pver,1,tcwat(1,1,begchunk,n3m2))

        call write_field_from_chunk(nrg,1,pver,1,lcwat(1,1,begchunk,n3  ))
        call write_field_from_chunk(nrg,1,pver,1,lcwat(1,1,begchunk,n3m2))
        !!
        !
        ! Comsrf module variables
        !

        call write_field_from_chunk(nrg,1,1,1,fsnt)

        call write_field_from_chunk(nrg,1,1,1,fsns)

        call write_field_from_chunk(nrg,1,1,1,flnt)
        call write_field_from_chunk(nrg,1,1,1,flns)

        do i = begchunk, endchunk
            tmpfield(:,i) = srfflx_state2d(i)%asdir(:)
        end do
        call write_field_from_chunk(nrg,1,1,1,tmpfield)
        do i = begchunk, endchunk
            tmpfield(:,i) = srfflx_state2d(i)%asdif(:)
        end do
        call write_field_from_chunk(nrg,1,1,1,tmpfield)
        do i = begchunk,endchunk
            tmpfield(:,i) = srfflx_state2d(i)%aldir(:)
        end do
        call write_field_from_chunk(nrg,1,1,1,tmpfield)
        do i = begchunk, endchunk
            tmpfield(:,i) = srfflx_state2d(i)%aldif(:)
        end do
        call write_field_from_chunk(nrg,1,1,1,tmpfield)


        call write_field_from_chunk(nrg,1,1,1,asdirice)
        call write_field_from_chunk(nrg,1,1,1,asdifice)
        call write_field_from_chunk(nrg,1,1,1,aldirice)
        call write_field_from_chunk(nrg,1,1,1,aldifice)
        call write_field_from_chunk(nrg,1,1,1,tsice)


        do i=begchunk,endchunk
            tmpfield(:,i) = srfflx_state2d(i)%lwup(:)
        end do
        call write_field_from_chunk(nrg,1,1,1,tmpfield)
        call write_field_from_chunk(nrg,1,1,1,landfrac)
        call write_field_from_chunk(nrg,1,1,1,landm)
        call write_field_from_chunk(nrg,1,1,1,sgh)
        do i=begchunk,endchunk
            tmpfield(:,i) = srfflx_state2d(i)%ts(:)
        end do
        call write_field_from_chunk(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            tmpfield3d(:,:,i) = surface_state2d(i)%tssub(:,:)
        end do
        call write_field_from_chunk(nrg,1,plevmx,1,tmpfield3d)
        call write_field_from_chunk(nrg,1,1,1,sicthk)
        call write_field_from_chunk(nrg,1,1,1,snowhland)

        call write_field_from_chunk(nrg,1,1,1,snowhice)



        do i=begchunk,endchunk
            ncol = get_ncols_p(i)
            tmpfield(:ncol,i) = surface_state2d(i)%flwds(:ncol)
        end do
        call write_field_from_chunk(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            ncol = get_ncols_p(i)
            tmpfield(:ncol,i) = surface_state2d(i)%sols(:ncol)
        end do
        call write_field_from_chunk(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            ncol = get_ncols_p(i)
            tmpfield(:ncol,i) = surface_state2d(i)%soll(:ncol)
        end do
        call write_field_from_chunk(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            ncol = get_ncols_p(i)
            tmpfield(:ncol,i) = surface_state2d(i)%solsd(:ncol)
        end do
        call write_field_from_chunk(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            ncol = get_ncols_p(i)
            tmpfield(:ncol,i) = surface_state2d(i)%solld(:ncol)
        end do
        call write_field_from_chunk(nrg,1,1,1,tmpfield)
        call write_field_from_chunk(nrg,1,1,1,trefmxav)
        call write_field_from_chunk(nrg,1,1,1,trefmnav)
        call write_field_from_chunk(nrg,1,1,1,icefrac)
        do i=begchunk,endchunk
            ncol = get_ncols_p(i)
            tmpfield(:ncol,i) = surface_state2d(i)%zbot(:ncol)
        end do
        call write_field_from_chunk(nrg,1,1,1,tmpfield)

        do i=begchunk,endchunk
            ncol = get_ncols_p(i)
            tmpfield(:ncol,i) = surface_state2d(i)%ubot(:ncol)
        end do
        call write_field_from_chunk(nrg,1,1,1,tmpfield)

        do i=begchunk,endchunk
            ncol = get_ncols_p(i)
            tmpfield(:ncol,i) = surface_state2d(i)%vbot(:ncol)
        end do
        call write_field_from_chunk(nrg,1,1,1,tmpfield)

        do i=begchunk,endchunk
            ncol = get_ncols_p(i)
            tmpfield(:ncol,i) = surface_state2d(i)%thbot(:ncol)
        end do
        call write_field_from_chunk(nrg,1,1,1,tmpfield)

        do i=begchunk,endchunk
            ncol = get_ncols_p(i)
            tmpfield(:ncol,i) = surface_state2d(i)%qbot(:ncol)
        end do
        call write_field_from_chunk(nrg,1,1,1,tmpfield)

        do i=begchunk,endchunk
            ncol = get_ncols_p(i)
            tmpfield(:ncol,i) = surface_state2d(i)%pbot(:ncol)
        end do
        call write_field_from_chunk(nrg,1,1,1,tmpfield)

        do i=begchunk,endchunk
            ncol = get_ncols_p(i)
            tmpfield(:ncol,i) = surface_state2d(i)%tbot(:ncol)
        end do
        call write_field_from_chunk(nrg,1,1,1,tmpfield)

      do i=begchunk,endchunk
       ncol = get_ncols_p(i)
       tmpfield(:ncol,i) = srfflx_parm2d(i)%ts(:ncol)
      end do
      call write_field_from_chunk(nrg,1,1,1,tmpfield)

      do i=begchunk,endchunk
       ncol = get_ncols_p(i)
       tmpfield(:ncol,i) = srfflx_parm2d(i)%asdir(:ncol)
      end do
      call write_field_from_chunk(nrg,1,1,1,tmpfield)

      do i=begchunk,endchunk
       ncol = get_ncols_p(i)
       tmpfield(:ncol,i) = srfflx_parm2d(i)%aldir(:ncol)
      end do
      call write_field_from_chunk(nrg,1,1,1,tmpfield)

      do i=begchunk,endchunk
       ncol = get_ncols_p(i)
       tmpfield(:ncol,i) = srfflx_parm2d(i)%asdif(:ncol)
      end do
      call write_field_from_chunk(nrg,1,1,1,tmpfield)

      do i=begchunk,endchunk
       ncol = get_ncols_p(i)
       tmpfield(:ncol,i) = srfflx_parm2d(i)%aldif(:ncol)
      end do
      call write_field_from_chunk(nrg,1,1,1,tmpfield)

      do i=begchunk,endchunk
       ncol = get_ncols_p(i)
       tmpfield(:ncol,i) = srfflx_parm2d(i)%wsx(:ncol)
      end do
      call write_field_from_chunk(nrg,1,1,1,tmpfield)

      do i=begchunk,endchunk
       ncol = get_ncols_p(i)
       tmpfield(:ncol,i) = srfflx_parm2d(i)%wsy(:ncol)
      end do
      call write_field_from_chunk(nrg,1,1,1,tmpfield)

      do i=begchunk,endchunk
       ncol = get_ncols_p(i)
       tmpfield(:ncol,i) = srfflx_parm2d(i)%lhf(:ncol)
      end do
      call write_field_from_chunk(nrg,1,1,1,tmpfield)

      do i=begchunk,endchunk
       ncol = get_ncols_p(i)
       tmpfield(:ncol,i) = srfflx_parm2d(i)%shf(:ncol)
      end do
      call write_field_from_chunk(nrg,1,1,1,tmpfield)

      do i=begchunk,endchunk
       ncol = get_ncols_p(i)
       tmpfield(:ncol,i) = srfflx_parm2d(i)%lwup(:ncol)
      end do
      call write_field_from_chunk(nrg,1,1,1,tmpfield)

      do i=begchunk,endchunk
        tmpfield(:ncol,i) = srfflx_parm2d(i)%cflx(:,1)
      end do
      call write_field_from_chunk(nrg,1,1,1,tmpfield)

      do i=begchunk,endchunk
       ncol = get_ncols_p(i)
       tmpfield(:ncol,i) = srfflx_parm2d(i)%tref(:ncol)
      end do
      call write_field_from_chunk(nrg,1,1,1,tmpfield)

      kvh_idx = pbuf_get_fld_idx('KVH')
      do i=begchunk,endchunk
       ncol = get_ncols_p(i)
       tmpfield3d2(:ncol,:pverp,i) = pbuf(kvh_idx)%fld_ptr(1,1:ncol,1:pverp,i,1)
      end do
      call write_field_from_chunk(nrg,1,pverp,1,tmpfield3d2)



        !
        !-----------------------------------------------------------------------
        ! Write the abs/ems restart dataset if necessary
        !-----------------------------------------------------------------------
        !
        if (aeres) then
            if (masterproc) then
                fname = interpret_filename_spec( rafilename_spec )
                pname = trim(get_archivedir('rest'))//fname
                call opnfil(fname, nrg2, 'u')
                write(nrg,iostat=ioerr) pname
                if (ioerr /= 0 ) then
                    write (6,*) 'WRITE ioerror ',ioerr,' on i/o unit = ',nrg
                    call endrun
                end if
            endif

            call write_field_from_chunk(nrg2, 1, pverp*pverp,1, abstot_3d(1,1,1,begchunk))
            call write_field_from_chunk(nrg2, 1, pver*4,     1, absnxt_3d(1,1,1,begchunk))
            call write_field_from_chunk(nrg2, 1, pverp,      1, emstot_3d(1,1,begchunk))

            if (masterproc) then
                close(nrg2)
                call putfil (fname, pname, mss_wpass, mss_irt, (.not. nlend) )
            end if
        end if

        return
    end subroutine write_restart_physics

    !#######################################################################

    subroutine read_restart_physics (nrg, nrg2, aeres )
        !
        ! Arguments
        !
        integer, intent(in) :: nrg
        integer, intent(in) :: nrg2
        integer :: kvh_idx

        logical, intent(in) :: aeres
        !
        ! Local workspace
        !
        real(r8) tmpfield(pcols,begchunk:endchunk)
        real(r8) tmpfield3d(pcols,plevmx,begchunk:endchunk)
        real(r8) tmpfield3d2(pcols,pverp,begchunk:endchunk)
        integer i                 ! loop index
        integer n3tmp             ! timestep index
        character*80  locfn       ! Local filename
        integer ioerr             ! I/O status
        !
        ! Buffer module variables
        !
        call initialize_buffer ()

        call read_chunk_from_field(nrg,1,1,1,pblht)
        call read_chunk_from_field(nrg,1,1,1,tpert)
        call read_chunk_from_field(nrg,1,pver,1,qrs)
        call read_chunk_from_field(nrg,1,pver,1,qrl)
        call read_chunk_from_field(nrg,1,pcnst+pnats,1,qpert)
        !
        ! cld, qcwat, and tcwat are physics things, but have dynamics time levels
        !
        !!
        call read_chunk_from_field(nrg,1,pver,1,cld(1,1,begchunk,n3  ))
        call read_chunk_from_field(nrg,1,pver,1,cld(1,1,begchunk,n3m2))

        call read_chunk_from_field(nrg,1,pver,1,qcwat(1,1,begchunk,n3  ))
        call read_chunk_from_field(nrg,1,pver,1,qcwat(1,1,begchunk,n3m2))

        call read_chunk_from_field(nrg,1,pver,1,tcwat(1,1,begchunk,n3  ))
        call read_chunk_from_field(nrg,1,pver,1,tcwat(1,1,begchunk,n3m2))

        call read_chunk_from_field(nrg,1,pver,1,lcwat(1,1,begchunk,n3  ))
        call read_chunk_from_field(nrg,1,pver,1,lcwat(1,1,begchunk,n3m2))
        !!
        !
        ! Comsrf module variables
        !
        call initialize_comsrf

        call read_chunk_from_field(nrg,1,1,1,fsnt)

        call read_chunk_from_field(nrg,1,1,1,fsns)

        call read_chunk_from_field(nrg,1,1,1,flnt)
        call read_chunk_from_field(nrg,1,1,1,flns)

        call read_chunk_from_field(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            srfflx_state2d(i)%asdir(:) = tmpfield(:,i)
        end do
        call read_chunk_from_field(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            srfflx_state2d(i)%asdif(:) = tmpfield(:,i)
        end do
        call read_chunk_from_field(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            srfflx_state2d(i)%aldir(:) = tmpfield(:,i)
        end do
        call read_chunk_from_field(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            srfflx_state2d(i)%aldif(:) = tmpfield(:,i)
        end do


        call read_chunk_from_field(nrg,1,1,1,asdirice)
        call read_chunk_from_field(nrg,1,1,1,asdifice)
        call read_chunk_from_field(nrg,1,1,1,aldirice)
        call read_chunk_from_field(nrg,1,1,1,aldifice)
        call read_chunk_from_field(nrg,1,1,1,tsice)


        call read_chunk_from_field(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            srfflx_state2d(i)%lwup(:) = tmpfield(:,i)
        end do
        call read_chunk_from_field(nrg,1,1,1,landfrac)
        call read_chunk_from_field(nrg,1,1,1,landm)
        call read_chunk_from_field(nrg,1,1,1,sgh)
        call read_chunk_from_field(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            srfflx_state2d(i)%ts(:) = tmpfield(:,i)
        end do
        call read_chunk_from_field(nrg,1,plevmx,1,tmpfield3d)
        do i=begchunk,endchunk
            surface_state2d(i)%tssub(:,:) = tmpfield3d(:,:,i)
        end do
        call read_chunk_from_field(nrg,1,1,1,sicthk)

        call read_chunk_from_field(nrg,1,1,1,snowhland)

        call read_chunk_from_field(nrg,1,1,1,snowhice)
        call read_chunk_from_field(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            surface_state2d(i)%flwds(:) = tmpfield(:,i)
        end do
        call read_chunk_from_field(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            surface_state2d(i)%sols(:) = tmpfield(:,i)
        end do
        call read_chunk_from_field(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            surface_state2d(i)%soll(:) = tmpfield(:,i)
        end do
        call read_chunk_from_field(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            surface_state2d(i)%solsd(:) = tmpfield(:,i)
        end do
        call read_chunk_from_field(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            surface_state2d(i)%solld(:) = tmpfield(:,i)
        end do
        call read_chunk_from_field(nrg,1,1,1,trefmxav)
        call read_chunk_from_field(nrg,1,1,1,trefmnav)
        call read_chunk_from_field(nrg,1,1,1,icefrac)

        call read_chunk_from_field(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            surface_state2d(i)%zbot(:) = tmpfield(:,i)
        end do

        call read_chunk_from_field(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            surface_state2d(i)%ubot(:) = tmpfield(:,i)
        end do

        call read_chunk_from_field(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            surface_state2d(i)%vbot(:) = tmpfield(:,i)
        end do

        call read_chunk_from_field(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            surface_state2d(i)%thbot(:) = tmpfield(:,i)
        end do

        call read_chunk_from_field(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            surface_state2d(i)%qbot(:) = tmpfield(:,i)
        end do

        call read_chunk_from_field(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            surface_state2d(i)%pbot(:) = tmpfield(:,i)
        end do

        call read_chunk_from_field(nrg,1,1,1,tmpfield)
        do i=begchunk,endchunk
            surface_state2d(i)%tbot(:) = tmpfield(:,i)
        end do

      call read_chunk_from_field(nrg,1,1,1,tmpfield)
      do i=begchunk,endchunk
	srfflx_parm2d(i)%ts(:) = tmpfield(:,i)
      end do

      call read_chunk_from_field(nrg,1,1,1,tmpfield)
      do i=begchunk,endchunk
	srfflx_parm2d(i)%asdir(:) = tmpfield(:,i)
      end do

      call read_chunk_from_field(nrg,1,1,1,tmpfield)
      do i=begchunk,endchunk
	srfflx_parm2d(i)%aldir(:) = tmpfield(:,i)
      end do

      call read_chunk_from_field(nrg,1,1,1,tmpfield)
      do i=begchunk,endchunk
	srfflx_parm2d(i)%asdif(:) = tmpfield(:,i)
      end do

      call read_chunk_from_field(nrg,1,1,1,tmpfield)
      do i=begchunk,endchunk
	srfflx_parm2d(i)%aldif(:) = tmpfield(:,i)
      end do

      call read_chunk_from_field(nrg,1,1,1,tmpfield)
      do i=begchunk,endchunk
	srfflx_parm2d(i)%wsx(:) = tmpfield(:,i)
      end do

      call read_chunk_from_field(nrg,1,1,1,tmpfield)
      do i=begchunk,endchunk
	srfflx_parm2d(i)%wsy(:) = tmpfield(:,i)
      end do

      call read_chunk_from_field(nrg,1,1,1,tmpfield)
      do i=begchunk,endchunk
	srfflx_parm2d(i)%lhf(:) = tmpfield(:,i)
      end do

      call read_chunk_from_field(nrg,1,1,1,tmpfield)
      do i=begchunk,endchunk
	srfflx_parm2d(i)%shf(:) = tmpfield(:,i)
      end do

      call read_chunk_from_field(nrg,1,1,1,tmpfield)
      do i=begchunk,endchunk
	srfflx_parm2d(i)%lwup(:) = tmpfield(:,i)
      end do

      call read_chunk_from_field(nrg,1,1,1,tmpfield)
      do i=begchunk,endchunk
       srfflx_parm2d(i)%cflx(:,1) = tmpfield(:,i)
      end do

      call read_chunk_from_field(nrg,1,1,1,tmpfield)
      do i=begchunk,endchunk
	srfflx_parm2d(i)%tref(:) = tmpfield(:,i)
      end do
      call read_chunk_from_field(nrg,1,pverp,1,fld_kvh)





        !
        !-----------------------------------------------------------------------
        ! Read the abs/ems restart dataset if necessary
        !-----------------------------------------------------------------------
        !
        call initialize_radbuffer ()
        if (aeres) then
            if (masterproc) then
                read(nrg,iostat=ioerr) pname
                if (ioerr /= 0 ) then
                    write (6,*) 'READ ioerror ',ioerr,' on i/o unit = ',nrg
                    call endrun
                end if
                call getfil (pname, locfn)
                call opnfil (locfn, nrg2, 'u')
            endif

            call read_chunk_from_field(nrg2, 1, pverp*pverp,1,abstot_3d(1,1,1,begchunk))
            call read_chunk_from_field(nrg2, 1, pver*4,     1,absnxt_3d(1,1,1,begchunk))
            call read_chunk_from_field(nrg2, 1, pverp,      1,emstot_3d(1,1,begchunk))

            if (masterproc) close(nrg2)
        end if

        return
    end subroutine read_restart_physics

    character(len=256) function get_abs_restart_filepath ( )
        !
        ! Return the full filepath to the abs-ems restart file
        !
        get_abs_restart_filepath = pname
    end function get_abs_restart_filepath

end module restart_physics
