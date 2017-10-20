# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/get_filename.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/get_filename.F90"
subroutine get_filename (fulpath, filname)

!----------------------------------------------------------------------- 
! 
! Purpose: 
! returns filename given full pathname
!
! Method: 
! 
! Author: Mariana Vertenstein
! 
!-----------------------------------------------------------------------
! $Id: get_filename.F90,v 1.1.12.1 2001/11/07 18:16:07 mvertens Exp $
!-----------------------------------------------------------------------

! ------------------------ arguments --------------------------------
  character(len=*), intent(in)  :: fulpath !full pathname
  character(len=*), intent(out) :: filname !full pathname
! -------------------------------------------------------------------

! ------------------------ local variables --------------------------
  integer i               !loop index
  integer klen            !length of fulpath character string
! -------------------------------------------------------------------

  klen = len_trim(fulpath)
  do i = klen, 1, -1
     if (fulpath(i:i) == '/') go to 10
  end do
  i = 0
10 filname = fulpath(i+1:klen)
  
  return
end subroutine get_filename


