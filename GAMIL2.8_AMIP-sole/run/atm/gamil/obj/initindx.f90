# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/initindx.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/initindx.F90"

subroutine initindx
!-----------------------------------------------------------------------
!
! Purpose: Registers constituents and determines index values.
!
! Method:
! <Describe the algorithm(s) used in the routine.>
! <Also include any applicable external references.>
!
! Author:    CSM Contact: M. Vertenstein, Aug. 1997
!            B.A. Boville, Oct 2001
!
!-----------------------------------------------------------------------
  use shr_kind_mod, only: r8 => shr_kind_r8
  use constituents, only: pcnst, ppcnst, cnst_add, advected, nonadvec, cnst_chk_dim, cnst_name
  use chemistry,    only: chem_register_cnst
  use physconst,    only: mwdry, cpair, mwh2o, cph2o
  use tracers
  use phys_buffer,  only: pbuf_init  !sxj-2008-11-10
  use MG,           only: stratiform_register  !sxj-2008-11-10
  use vertical_diffusion, only: vd_register  !sxj
  USE pmgrid,  ONLY: masterproc,iam !--sxj--debug-test

  implicit none
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
# 28 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/initindx.F90" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/RK_or_MG.h" 1
 
  character(len=2)  RK_or_MG
  parameter (RK_or_MG='MG')

# 29 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/initindx.F90" 2
!---------------------------Local variables-----------------------------
!
  integer m            ! loop index
  integer mm           ! constituent index

  logical, parameter :: cldw_adv=.false.  ! true => cloud water is treated as advected tracer

  character*3 trnum   ! Advected species number (Character)

!
!-----------------------------------------------------------------------
! Register constituents and tracers and set starting indexes
! Note that public starting indexes should not be required, these should be
! private to the package (module) implementing the tracer and may be obtained
! by calling cnst_get_ind
!-----------------------------------------------------------------------
!
  ! Initialize physics buffer
  call pbuf_init() !sxj-2008-11-10

! Register water vapor
  call cnst_add('Q', advected, mwh2o, cph2o, 1.0E-12_r8, ixmoist, &
                longname='Specific humidity')
!
! Register advected tracers and determine starting index
  do m = 1, nusr_adv
     write(unit=trnum,fmt='(i3)') m+100
     call cnst_add('ADV'//trnum(2:3), advected, mwdry, cpair, 0._r8, mm, &
                  longname='Advected tracer no. '//trnum(2:3))
     if (m == 1) ixuadv = mm
  end do
!
! Register non-advected tracers and determine starting index (shouldn't need
  do m = 1, nusr_nad
     write(unit=trnum,fmt='(i3)') m+100
     call cnst_add('NAD'//trnum(2:3), nonadvec, mwdry, cpair, 0._r8, mm, &
                  longname='Non-advected tracer no. '//trnum(2:3))
     if (m == 1) ixunad = mm
  end do
!
!
! Register chemical constituents
  if (trace_gas) then
     call chem_register_cnst
  endif


  if (RK_or_MG=='MG') then
! Register MG stratiform constituents ! sxj-2008-11-10
! cloud water ice  /two-moment
     call stratiform_register
	 call vd_register
	 if (masterproc) write(6,*) "stratiform_register***sxj********"
  elseif (RK_or_MG=='RK') then
! Register cloud water and determine index (either advected or non-adv).
     if (cldw_adv) then  !(cldw_adv false)  pcnst pnats
         call cnst_add('CWAT', advected, mwdry, cpair, 0._r8, ixcldw, &
                    longname='Total Grid box averaged Condensate Amount (liquid + ice)')
     else
         call cnst_add('CWAT', nonadvec, mwdry, cpair, 0._r8, ixcldw, &
                    longname='Total Grid box averaged Condensate Amount (liquid + ice)')
     endif
! add 3 nonadvec constituent   make pnad(last non-advect tracer number equal pcnst<1>+pnats<4>)
     call cnst_add('TEST1', nonadvec, mwdry, cpair, 0._r8, mm)
	 call cnst_add('TEST2', nonadvec, mwdry, cpair, 0._r8, mm)
	 call cnst_add('TEST3', nonadvec, mwdry, cpair, 0._r8, mm)
  endif
!
! Register advected test tracers and determine starting index
  if (trace_test1 .or. trace_test2 .or. trace_test3) &
       call cnst_add('TEST1', advected, mwdry, cpair, 0._r8, ixtrct)
  if (trace_test2 .or. trace_test3) &
       call cnst_add('TEST2', advected, mwdry, cpair, 0._r8, mm)
  if (trace_test3) &
       call cnst_add('TEST3', advected, mwdry, cpair, 0._r8, mm)
!
! All tracers registered, check that the dimensions are correct
  call cnst_chk_dim
!
! Set default names for non-water advected and non-advected tracers
! Set names of advected and non-advected tracer diagnostics
!
  do m=1,ppcnst
     dcconnam(m) = 'DC'//cnst_name(m)
     sflxnam(m)  = 'SF'//cnst_name(m)
  end do
  do m=1,pcnst
     hadvnam(m)  = 'HA'//cnst_name(m)
     vadvnam(m)  = 'VA'//cnst_name(m)
     fixcnam(m)  = 'DF'//cnst_name(m)
     tendnam(m)  = 'TE'//cnst_name(m)
     tottnam(m)  = 'TA'//cnst_name(m)
  end do

  return
end subroutine initindx
