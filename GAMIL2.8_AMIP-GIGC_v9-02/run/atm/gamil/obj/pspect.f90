# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/pspect.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/pspect.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/pspect.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/pspect.F90" 2

module pspect
!----------------------------------------------------------------------- 
! 
! Purpose: Parameters related to spectral domain
! 
! Author: CCM Core Group
! 
!-----------------------------------------------------------------------
   integer, parameter :: ptrm = 42                ! M truncation parameter
   integer, parameter :: ptrn = 42                ! N truncation parameter
   integer, parameter :: ptrk = 42                ! K truncation parameter
                                                   
   integer, parameter :: pmax = ptrn+1              ! number of diagonals
   integer, parameter :: pmaxp = pmax+1             ! Number of diagonals plus 1
   integer, parameter :: pnmax = ptrk+1             ! Number of values of N
   integer, parameter :: pmmax = ptrm+1             ! Number of values of M
   integer, parameter :: par0 = ptrm+ptrn-ptrk      ! intermediate parameter
   integer, parameter :: par2 = par0*(par0+1)/2     ! intermediate parameter
   integer, parameter :: pspt = (ptrn+1)*pmmax-par2 ! Total num complex spectral coeffs retained
   integer, parameter :: psp = 2*pspt               ! 2*pspt (real) size of coeff array per level

end module pspect
