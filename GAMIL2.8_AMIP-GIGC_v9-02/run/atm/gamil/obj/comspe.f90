# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/comspe.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/comspe.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/comspe.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/comspe.F90" 2

module comspe

!----------------------------------------------------------------------- 
! 
! Purpose: Spectral space arrays
! 
! Method: 
! 
! Author: CCM Core Group
! $Author: erik $
! $Id: comspe.F90,v 1.2.6.1 2002/06/15 13:47:39 erik Exp $
! 
!-----------------------------------------------------------------------

  use shr_kind_mod, only: r8 => shr_kind_r8
  use infnan
  use pmgrid, only: plev, plat
  use pspect

  implicit none
!
! $Id: comspe.F90,v 1.2.6.1 2002/06/15 13:47:39 erik Exp $
! $Author: erik $
!
! Spectral space arrays
!
  real(r8) :: vz(psp,plev) = inf      ! Vorticity spectral coefficients
  real(r8) :: d(psp,plev)  = inf      ! Divergence spectral coefficients
  real(r8) :: t(psp,plev)  = inf      ! Temperature spectral coefficients
  real(r8) :: alps(psp)    = inf      ! Log-pressure spectral coefficients

  integer :: ncutoff       = bigint   ! Break-even point for vector lengths in GRCALC
  integer :: nalp(pmax)    = bigint   ! Pointer into polynomial arrays

  integer :: begm(0:plat-1) = bigint  ! Starting Fourier wavenumber owned by MPI task
  integer :: endm(0:plat-1) = bigint  ! Ending Fourier wavenumber owned by MPI task










  integer :: nstart(pmmax) = bigint   ! Starting indices for spectral arrays (real)
  integer :: nlen(pmmax)   = bigint   ! Length vectors for spectral arrays

  real(r8) :: alp(pspt,plat/2)  = inf ! Legendre polynomials
  real(r8) :: dalp(pspt,plat/2) = inf ! Legendre polynomial derivatives

end module comspe
