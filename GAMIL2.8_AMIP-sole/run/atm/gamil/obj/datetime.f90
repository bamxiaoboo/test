# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/datetime.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/datetime.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/datetime.F90" 2
   subroutine datetime(cdate, ctime) 
!-----------------------------------------------------------------------
!
! Purpose:
!
!  A generic Date and Time routine
!
! Author: CCM Core group
!
!-----------------------------------------------------------------------
!
! $Id: datetime.F90,v 1.1 2000/06/14 19:24:46 erik Exp $
!
!-----------------------------------------------------------------------
   implicit none
!-----------------------------------------------------------------------
!
!-----------------------------Arguments---------------------------------
   character , intent(out) :: cdate*8 
   character , intent(out) :: ctime*8 
!-----------------------------------------------------------------------
!
!---------------------------Local Variables------------------------------
   integer, dimension(8) :: values 
   character :: date*8, time*10, zone*5 
!-----------------------------------------------------------------------
 
   call date_and_time (date, time, zone, values) 
   cdate(1:2) = date(5:6) 
   cdate(3:3) = '/' 
   cdate(4:5) = date(7:8) 
   cdate(6:6) = '/' 
   cdate(7:8) = date(3:4) 
   ctime(1:2) = time(1:2) 
   ctime(3:3) = ':' 
   ctime(4:5) = time(3:4) 
   ctime(6:6) = ':' 
   ctime(7:8) = time(5:6) 
 
   return  
   end subroutine datetime 
 