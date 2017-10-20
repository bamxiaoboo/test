# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/clm_csmMod.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/clm_csmMod.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/clm_csmMod.F90" 2

# 1 "./preproc.h" 1






 
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/clm_csmMod.F90" 2

module clm_csmMod

!-----------------------------------------------------------------------
!
! Purpose:
! Set of routines that define communication between the land model
! and the flux coupler. The order of sends/receives is as follows:
!  - receive orbital data from coupler (csm_recvorb)
!  - send control data (grids and masks) to coupler (csm_sendcontrol)
!    land grid does not have valid data, runoff grid does
!  - receive valid land grid from flux coupler (csm_recvgrid)
!  - send compressed runoff information to flux coupler (csm_sendrunoff)
!  - send first land model data to flux coupler (csm_send_alb)
!  - start normal send/recv communication pattern
!      => csm_dosndrcv
!      => csm_recv
!      => csm_flxave
!      => csm_send
!
! Method:
!
! Author: Mariana Vertenstein
!
!-----------------------------------------------------------------------
! $Id: clm_csmMod.F90,v 1.12.2.7 2002/06/15 13:50:27 erik Exp $
!-----------------------------------------------------------------------

# 1291 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/clm_csmMod.F90"

end module clm_csmMod
