# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/clm_varpar.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/clm_varpar.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/clm_varpar.F90" 2

# 1 "./preproc.h" 1






 
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main/clm_varpar.F90" 2

module clm_varpar

  use shr_kind_mod, only: r8 => shr_kind_r8
  implicit none

!----------------------------------------------------------------------- 
! 
! Purpose: 
! land surface model array dimensions
! 
! Method: 
! 
! Author: Mariana Vertenstein
! 
!-----------------------------------------------------------------------
! $Id: clm_varpar.F90,v 1.4.6.3.6.1 2002/10/03 20:07:35 erik Exp $
!-----------------------------------------------------------------------

! Define land surface 2-d grid. 
! This sets the model resolution according to cpp directives 
! 128 and 60 in preproc.h. 

  integer, parameter :: lsmlon = 128  !maximum number of longitude points on model grid
  integer, parameter :: lsmlat = 60  !number of latitude points on model grid

! Define maximum number of PFT patches per grid cell and set
! patch number for urban, lake, wetland, and glacier patches

  integer, parameter :: maxpatch_pft = 4                !maximum number of PFT subgrid patches per grid cell
  integer, parameter :: npatch_urban = maxpatch_pft + 1 !urban   patch number: 1 to maxpatch
  integer, parameter :: npatch_lake  = npatch_urban + 1 !lake    patch number: 1 to maxpatch
  integer, parameter :: npatch_wet   = npatch_lake  + 1 !wetland patch number: 1 to maxpatch
  integer, parameter :: npatch_gla   = npatch_wet   + 1 !glacier patch number: 1 to maxpatch
  integer, parameter :: maxpatch     = npatch_gla       !maximum number of subgrid patches per grid cell

! Define history file parameters

  integer , parameter :: maxhist      =   3  !max number of history files
  integer , parameter :: maxflds      = 200  !max number of fields (active and inacative) in list
  integer , parameter :: max_slevflds =  75  !max number of active single-level fields
  integer , parameter :: max_mlevflds =  10  !max number of active multi-level fields 
  integer , parameter :: maxalflds = max_slevflds + max_mlevflds !max number of active fields (all levels)

! Define number of level parameters

  integer, parameter :: nlevsoi     =  10   !number of soil layers
  integer, parameter :: nlevlak     =  10   !number of lake layers
  integer, parameter :: nlevsno     =   5   !maximum number of snow layers

! Define miscellaneous parameters

  integer, parameter :: numwat      =   5   !number of water types (soil, ice, 2 lakes, wetland)
  integer, parameter :: numpft      =  16   !number of plant types
  integer, parameter :: numcol      =   8   !number of soil color types
  integer, parameter :: numrad      =   2   !number of solar radiation bands: vis, nir
  integer, parameter :: ndst        =   4   !number of dust size classes
  integer, parameter :: dst_src_nbr =   3   !number of size distns in src soil
  integer, parameter :: nvoc        =   5   !number of voc categories

! Define parameters for RTM river routing model

  integer, parameter :: rtmlon = 720  !# of rtm longitudes
  integer, parameter :: rtmlat = 360  !# of rtm latitudes

end module clm_varpar

