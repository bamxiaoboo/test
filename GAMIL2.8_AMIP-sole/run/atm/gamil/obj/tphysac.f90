# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/tphysac.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/tphysac.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/tphysac.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/tphysac.F90" 2

subroutine tphysac (ztodt,   pblh,    qpert,   tpert,  shf,  &
                    taux,    tauy,    cflx,    sgh,    lhf,  &
                    landfrac,snowh,   tref,    precc,  precl,  &
                    tin,     state,   tend,    ocnfrac )
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Tendency physics after coupling to land, sea, and ice models.
! Computes the following:
!   o Radon surface flux and decay (optional)
!   o Vertical diffusion and planetary boundary layer
!   o Dry deposition for sulfur cycle (optional)
!   o Multiple gravity wave drag
! 
! Method: 
! <Describe the algorithm(s) used in the routine.> 
! <Also include any applicable external references.> 
! 
! Author: CCM1, CMS Contact: J. Truesdale
! 
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8
   use ppgrid,             only: pcols, pver
   use chemistry,          only: chem_driver
   use gw_drag,            only: gw_intr
   use vertical_diffusion, only: vd_intr
   use physics_types,      only: physics_state, physics_tend, physics_ptend, physics_update, physics_ptend_init
   use constituents,       only: ppcnst, qmin
   use tracers,            only: ixtrct
   use physconst,          only: zvir, gravit, rhoh2o, latvap

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
# 38 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/tphysac.F90" 2
!
! Arguments
!
   real(r8), intent(in) :: ztodt                  ! Two times model timestep (2 delta-t)
   real(r8), intent(in) :: landfrac(pcols)        ! Land fraction
   real(r8), intent(in) :: ocnfrac(pcols)         ! Land fraction
   real(r8), intent(in) :: snowh(pcols)           ! snow depth (liquid water equivalent)
   real(r8), intent(in) :: tref(pcols)            ! 2m air temperature
   real(r8), intent(in) :: precc(pcols)           ! convective precipitation
   real(r8), intent(in) :: precl(pcols)           ! large-scale precipitation
   real(r8), intent(out) :: pblh(pcols)           ! Planetary boundary layer height
   real(r8), intent(out) :: qpert(pcols,ppcnst)   ! Moisture/constit. perturbation (PBL)
   real(r8), intent(out) :: tpert(pcols)          ! Temperature perturbation (PBL)
   real(r8), intent(inout) :: shf(pcols)          ! Sensible heat flux (w/m^2)
   real(r8), intent(in) :: taux(pcols)            ! X surface stress (zonal)
   real(r8), intent(in) :: tauy(pcols)            ! Y surface stress (meridional)
   real(r8), intent(inout) :: cflx(pcols,ppcnst)  ! Surface constituent flux (kg/m^2/s)
   real(r8), intent(in) :: sgh(pcols)             ! Std. deviation of orography for gwd
   real(r8), intent(inout) :: lhf(pcols)          ! Latent heat flux (w/m^2)
   real(r8), intent(in) :: tin(pcols, pver) ! input T, to compute FV output T

   type(physics_state), intent(inout) :: state
   type(physics_tend ), intent(inout) :: tend
!
!---------------------------Local workspace-----------------------------
!
   type(physics_ptend) :: ptend                  ! indivdual parameterization tendencies

   integer :: lchnk                                ! chunk identifier
   integer :: ncol                                 ! number of atmospheric columns
   integer i                 ! Longitude, level indices
   integer :: yr, mon, day, tod       ! components of a date

   logical :: labort                            ! abort flag

   real(r8) tvm(pcols,pver)           ! virtual temperature
   real(r8) prect(pcols)              ! total precipitation
   real(r8) surfric(pcols)              ! surface friction velocity
   real(r8) obklen(pcols)             ! Obukhov length

!! (wanhui 2003.06.11)

   real(r8) dudtm,dvdtm,dtdtm
   integer  iic,kk
!!
!
!-----------------------------------------------------------------------
   lchnk = state%lchnk
   ncol  = state%ncol
!
! accumulate fluxes into net flux array
!
   do i=1,ncol
      tend%flx_net(i) = tend%flx_net(i) + shf(i) + (precc(i) + precl(i))*latvap*rhoh2o
   end do

! Convert mixing ratio of non-water tracers to mass fraction of total
! atmospheric mass (Overwrite non-water portions of q).

   if (ppcnst > 1) then
      call mr2mf (lchnk, ncol, state%q)
   end if

! Initialize parameterization tendency structure

   call physics_ptend_init(ptend)

! Check if latent heat flux exceeds the total moisture content of the
! lowest model layer, thereby creating negative moisture.

   call qneg4('TPHYSAC '       ,lchnk               ,ncol  ,ztodt ,          &
              state%q(1,pver,1),state%rpdel(1,pver) ,shf ,lhf ,cflx(1,1) )

!===================================================
! Source/sink terms for advected tracers.
!===================================================

   if ( trace_test1 .or. trace_test2 .or. trace_test3 ) then
      write(*,*) '!! trace_test'
      call rnsfwcrp( lchnk, ncol, landfrac, cflx(:,ixtrct))
      call rndecay( lchnk, ncol, state%q(:,:,ixtrct), ztodt, ptend%q(:,:,ixtrct))
      ptend%lq(ixtrct) = .TRUE.
      if (trace_test3) state%q(:ncol,pver,ixtrct+2) =  0.
   end if

! Advected greenhouse trace gases:

   if (trace_gas) call chem_driver (state, ptend, cflx, ztodt)
   if (trace_gas) write(*,*) '!!  trace_gas = .true.'

! Add tendencies to cummulative model tendencies and update profiles

   call physics_update (state, tend, ptend, ztodt)

!===================================================
! Vertical diffusion/pbl calculation
! Call vertical diffusion code (pbl, free atmosphere and molecular)
!===================================================

   call vd_intr (ztodt    ,state    ,taux     ,tauy     , shf    ,&
                 cflx     ,pblh     ,tpert    ,qpert    , surfric  ,&
                 obklen   ,ptend    )

   call physics_update (state, tend, ptend, ztodt)

!!------------------------------------------------------------------------
!!(wanhui 2003.06.11)
!
!      do kk=1,pver
!       do iic=1,pcols
!
!          dudtm = tend%dudt(iic,kk)
!          dvdtm = tend%dvdt(iic,kk)
!          dtdtm = tend%dtdt(iic,kk)
!
!          if ( abs(dudtm) > 1.0d-3 ) then
!             write(161,55) 'chunk',lchnk,'col',iic,'lev',kk,':dudt=',dudtm
!          endif
!
!          if ( abs(dvdtm) > 1.0d-3 ) then
!             write(162,55) 'chunk',lchnk,'col',iic,'lev',kk,':dvdt=',dvdtm
!          endif
!
!          if ( abs(dtdtm) > 1.0d-3 ) then
!             write(163,55) 'chunk',lchnk,'col',iic,'lev',kk,':dtdt=',dtdtm
!          endif
!
!       enddo
!      enddo
!
!55    format(1x,a6,i4,a4,i3,a5,i3,a8,e25.18)


!===================================================
! Gravity wave drag
!===================================================

   call gw_intr (state   ,sgh     ,pblh    ,ztodt   , ptend )
   call physics_update (state, tend, ptend, ztodt)

!!------------------------------------------------------------------------
!!(wanhui 2003.06.11)
!
!      do kk=1,pver
!       do iic=1,pcols
!
!          dudtm = tend%dudt(iic,kk)
!          dvdtm = tend%dvdt(iic,kk)
!          dtdtm = tend%dtdt(iic,kk)
!
!          if ( abs(dudtm) > 1.0d-3 ) then
!              write(171,55) 'chunk',lchnk,'col',iic,'lev',kk,':dudt=',dudtm
!          endif
!
!          if ( abs(dvdtm) > 1.0d-3 ) then
!             write(172,55) 'chunk',lchnk,'col',iic,'lev',kk,':dvdt=',dvdtm
!          endif
!
!          if ( abs(dtdtm) > 1.0d-3 ) then
!             write(173,55) 'chunk',lchnk,'col',iic,'lev',kk,':dtdt=',dtdtm
!          endif
!
!       enddo
!      enddo
!


!*** BAB's FV kludge

   state%t(:ncol,:pver) = tin(:ncol,:pver) + ztodt*tend%dtdt(:ncol,:pver)

   if (aqua_planet) then
      labort = .false.
      do i=1,ncol
         if (ocnfrac(i) /= 1.) labort = .true.
      end do
      if (labort) then
         write(6,*) 'ERROR:  grid contains non-ocean point'
         call endrun ()
      endif
   endif
!
! Convert mass fractions of non-water tracers back to mixing ratios.
! (Overwrite non-water portions of q).
!
   if (ppcnst > 1) then
      call mf2mr (lchnk, ncol, state%q)
   end if

   return
end subroutine tphysac
