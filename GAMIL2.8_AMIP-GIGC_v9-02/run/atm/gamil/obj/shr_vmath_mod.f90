# 1 "/data3/work/yuxinzhu/test/model_platform/models/libs/shr/shr_vmath_mod.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/libs/shr/shr_vmath_mod.F90"
!===============================================================================
! CVS: $Id: shr_vmath_mod.F90,v 1.2 2003/11/22 00:27:09 tcraig Exp $
! CVS: $Source: /fs/cgd/csm/models/CVS.REPOS/shared/csm_share/shr/shr_vmath_mod.F90,v $
! CVS: $Name: ccsm3_0_1_beta14 $
!===============================================================================
! PURPOSE: 
!   provides a uniform, platform-independent API for vector math functions
!===============================================================================

module shr_vmath_mod

   !----------------------------------------------------------------------------
   ! routines that evaluate various math functions for vector arguments
   ! intended to provide platform independent access to vendor optimized code
   !----------------------------------------------------------------------------

   use shr_kind_mod

   implicit none

   private
   public :: shr_vmath_sqrt, &
      shr_vmath_exp, shr_vmath_log, &
      shr_vmath_sin, shr_vmath_cos, &
      shr_vmath_rsqrt, shr_vmath_div

   contains

!===============================================================================

subroutine shr_vmath_sqrt(X, Y, n)

   !----- arguments ---
   integer(SHR_KIND_IN),intent(in)  ::   n  ! vector length
   real   (SHR_KIND_R8),intent(in)  :: X(n) ! input vector argument
   real   (SHR_KIND_R8),intent(out) :: Y(n) ! output vector argument

!-------------------------------------------------------------------------------
! PURPOSE: sqrt for vector arguments, optimized on different platforms
!-------------------------------------------------------------------------------


   Y = sqrt(X)
# 62 "/data3/work/yuxinzhu/test/model_platform/models/libs/shr/shr_vmath_mod.F90"

end subroutine shr_vmath_sqrt

!===============================================================================

subroutine shr_vmath_rsqrt(X, Y, n)

   !----- arguments ---
   integer(SHR_KIND_IN),intent(in)  ::   n  ! vector length
   real   (SHR_KIND_R8),intent(in)  :: X(n) ! input vector argument
   real   (SHR_KIND_R8),intent(out) :: Y(n) ! output vector argument

!-------------------------------------------------------------------------------
! PURPOSE: sqrt for vector arguments, optimized on different platforms
!-------------------------------------------------------------------------------


   Y = 1.0/sqrt(X)
# 90 "/data3/work/yuxinzhu/test/model_platform/models/libs/shr/shr_vmath_mod.F90"

end subroutine shr_vmath_rsqrt

!===============================================================================

subroutine shr_vmath_exp(X, Y, n)

   !----- arguments ---
   integer(SHR_KIND_IN),intent(in)  ::   n  ! vector length
   real   (SHR_KIND_R8),intent(in)  :: X(n) ! input vector argument
   real   (SHR_KIND_R8),intent(out) :: Y(n) ! output vector argument

!-------------------------------------------------------------------------------
! PURPOSE: exp for vector arguments, optimized on different platforms
!-------------------------------------------------------------------------------


   Y = exp(X)
# 126 "/data3/work/yuxinzhu/test/model_platform/models/libs/shr/shr_vmath_mod.F90"

end subroutine shr_vmath_exp

!===============================================================================

subroutine shr_vmath_div(X, Y, Z, n)
   !----- arguments ---
   integer(SHR_KIND_IN),intent(in)  ::   n  ! vector length
   real   (SHR_KIND_R8),intent(in)  :: X(n) ! input vector argument
   real   (SHR_KIND_R8),intent(in)  :: Y(n) ! input vector argument
   real   (SHR_KIND_R8),intent(out) :: Z(n) ! output vector argument


   integer :: i
   do i=1,n
      Z(i) = X(i)/Y(i)
   enddo
# 153 "/data3/work/yuxinzhu/test/model_platform/models/libs/shr/shr_vmath_mod.F90"
   return
 end subroutine shr_vmath_div

!===============================================================================

subroutine shr_vmath_log(X, Y, n)

   !----- arguments ---
   integer(SHR_KIND_IN),intent(in)  ::   n  ! vector length
   real   (SHR_KIND_R8),intent(in)  :: X(n) ! input vector argument
   real   (SHR_KIND_R8),intent(out) :: Y(n) ! output vector argument

!-------------------------------------------------------------------------------
! PURPOSE: log for vector arguments, optimized on different platforms
!-------------------------------------------------------------------------------


   Y = log(X)
# 189 "/data3/work/yuxinzhu/test/model_platform/models/libs/shr/shr_vmath_mod.F90"

end subroutine shr_vmath_log

!===============================================================================

subroutine shr_vmath_sin(X, Y, n)

   !----- arguments ---
   integer(SHR_KIND_IN),intent(in)  ::   n  ! vector length
   real   (SHR_KIND_R8),intent(in)  :: X(n) ! input vector argument
   real   (SHR_KIND_R8),intent(out) :: Y(n) ! output vector argument

!-------------------------------------------------------------------------------
! PURPOSE: sin for vector arguments, optimized on different platforms
!-------------------------------------------------------------------------------


   Y = sin(X)
# 225 "/data3/work/yuxinzhu/test/model_platform/models/libs/shr/shr_vmath_mod.F90"

end subroutine shr_vmath_sin

!===============================================================================

subroutine shr_vmath_cos(X, Y, n)

   !----- arguments ---
   integer(SHR_KIND_IN),intent(in)  ::   n  ! vector length
   real   (SHR_KIND_R8),intent(in)  :: X(n) ! input vector argument
   real   (SHR_KIND_R8),intent(out) :: Y(n) ! output vector argument

!-------------------------------------------------------------------------------
! PURPOSE: cos for vector arguments, optimized on different platforms
!-------------------------------------------------------------------------------


   Y = cos(X)
# 261 "/data3/work/yuxinzhu/test/model_platform/models/libs/shr/shr_vmath_mod.F90"

end subroutine shr_vmath_cos

!===============================================================================

end module shr_vmath_mod
