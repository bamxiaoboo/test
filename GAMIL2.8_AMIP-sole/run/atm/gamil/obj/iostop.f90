# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/iostop.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/iostop.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/iostop.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/iostop.F90" 2

subroutine iostop (iostat  ,nunit   ,nrec    ,clabel)
!----------------------------------------------------------------------- 
! 
! Purpose: Explain the CRAY FORTRAN I/O error, then call endrun
! 
! Method: Print input diagnostic message.  If Cray, also call "explain function to
!         provide further diagnosis
! 
! Author: CCM Core Group
! 
!-----------------------------------------------------------------------
   implicit none
!-----------------------------------------------------------------------
!
! Input arguments
!
   integer, intent(in) :: iostat           ! I/O error number from err=iostat option
   integer, intent(in) :: nrec             ! Number of current record (ignored if <=0)
   integer, intent(in) :: nunit            ! I/O Unit number
   character (len=*), intent(in) :: clabel ! Users written diagnostic
!
!---------------------------Local variables-----------------------------
!
   integer i              ! index
# 37 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/iostop.F90"
!
!-----------------------------------------------------------------------
!
   if (iostat /= 0) then
      write (6,*) 'IOSTOP:',('*',i=1,30),'  I/O ERROR  ',('*',i=1,29)
      write (6,*) '       ',clabel 
      if (nrec.ge.1) then
         write (6,*) 'I/O Unit = ',nunit,'   Record number = ',nrec,'  Error number = ',iostat
      else
         write (6,*) 'I/O Unit = ',nunit,'   Error number = ',iostat
      end if
      if (iostat.gt.0) then





      else
         write (6,*) 'End Of File (EOF) was encountered.'
      end if
      call endrun
   end if
!
   return
end subroutine iostop

