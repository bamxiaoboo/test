# 1 "/data3/work/yuxinzhu/test/model_platform/models/libs/shr/shr_kind_mod.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/libs/shr/shr_kind_mod.F90"
!===============================================================================
! CVS: $Id: shr_kind_mod.F90,v 1.2 2003/11/22 00:27:08 tcraig Exp $
! CVS: $Source: /fs/cgd/csm/models/CVS.REPOS/shared/csm_share/shr/shr_kind_mod.F90,v $
! CVS: $Name: ccsm3_0_1_beta14 $
!===============================================================================

MODULE shr_kind_mod

   !----------------------------------------------------------------------------
   ! precision/kind constants add data public
   !----------------------------------------------------------------------------
   public
   integer,parameter :: SHR_KIND_R16= selected_real_kind(24) ! 16 byte real
   integer,parameter :: SHR_KIND_R8 = selected_real_kind(12) ! 8 byte real
   integer,parameter :: SHR_KIND_R4 = selected_real_kind( 6) ! 4 byte real
   integer,parameter :: SHR_KIND_RN = kind(1.0)              ! native real
   integer,parameter :: SHR_KIND_I8 = selected_int_kind (13) ! 8 byte integer
   integer,parameter :: SHR_KIND_I4 = selected_int_kind ( 6) ! 4 byte integer
   integer,parameter :: SHR_KIND_IN = kind(1)                ! native integer
   integer,parameter :: SHR_KIND_CL = 256                    ! long char
   integer,parameter :: SHR_KIND_CS = 80                     ! short char

END MODULE shr_kind_mod
