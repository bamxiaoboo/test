# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/couple/c_coupler/coupling_chemistry_model_mod.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/couple/c_coupler/coupling_chemistry_model_mod.F90"
!***************************************************************
!  This is a source file of GAMIL, which registers all variables
!  with chemistry model into C-Coupler library for coupling. 
!  This file was initially finished by Dr. Li Liu. If you have 
!  any problem, please contact Dr. Li Liu via 
!  liuli-cess@tsinghua.edu.cn
!***************************************************************



# 1 "./misc.h" 1
# 11 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/couple/c_coupler/coupling_chemistry_model_mod.F90" 2

# 1 "./params.h" 1
# 12 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/couple/c_coupler/coupling_chemistry_model_mod.F90" 2


module coupling_chemistry_model_mod

    use shr_kind_mod, only: r8 => shr_kind_r8
    use ppgrid
    use phys_grid,    only: read_chunk_from_field, write_field_from_chunk, get_ncols_p
    use pmgrid,       only: masterproc
    use prognostics,  only: ptimelevels, n3, n3m2
    use buffer
    use radae,        only: abstot_3d, absnxt_3d, emstot_3d, initialize_radbuffer
    use comsrf, only: surface_state2d, srfflx_state2d
    use ioFileMod
    use phys_buffer
    use CCPL_interface_mod

    implicit none
    !
    ! Public interfaces
    !

    type, private :: fld_container_for_coupling_chem
        character(16)        name
        integer              num_lev
        real(r8), pointer :: fld_buf(:,:,:)
    end type fld_container_for_coupling_chem

    integer, private, parameter                    :: max_num_chem_flds = 128
    integer, private                               :: num_registered_flds_for_chem=0
    type(fld_container_for_coupling_chem), private :: registered_flds_for_chem(max_num_chem_flds)
    integer, public                                :: gamil_comp_id

    integer, private          :: decomp_id, grid_h2d_id, grid_v1d_id
    integer, private          :: grid_3d_id, grid_mid_3d_id

    real, private,allocatable :: PRECCON_array(:,:) 
    real, private,allocatable :: PRECTOT_array(:,:) 
    real, private,allocatable :: PRECSNO_array(:,:) 
    real, private,allocatable :: RADLWG_array(:,:) 
    real, private,allocatable :: RADSWG_array(:,:) 
    real, private,allocatable :: FRLAKE_array(:,:) 
    real, private,allocatable :: FRLANDIC_array(:,:) 

    integer, allocatable :: flds_id(:)

    interface out_fld_for_coupling_chem ; module procedure &
        out_fld_for_coupling_chem_3D, &
        out_fld_for_coupling_chem_2D, &
        out_fld_for_coupling_chem_2D_lchnk, &
        out_fld_for_coupling_chem_1D_lchnk
    end interface




CONTAINS


    subroutine register_gamil_component(comm)
        integer, intent(inout) :: comm
        gamil_comp_id = CCPL_register_component(-1, "gamil", "atm", comm, annotation = "register atm model gamil")
    end subroutine register_gamil_component

    subroutine register_grids_decomps
        use pmgrid
        use ppgrid
        use phys_grid
        use rgrid,          only: nlon                                                  ! reduced grid
        use commap
        use dycore, only: dycore_is
        use shr_const_mod,  only: shr_const_spval
        use CCPL_interface_mod
        implicit none

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/comhyb.h" 1
!----------------------------------------------------------------------- 
! 
! Purpose: Hybrid level definitions: p = a*p0 + b*ps
!          interfaces   p(k) = hyai(k)*ps0 + hybi(k)*ps
!          midpoints    p(k) = hyam(k)*ps0 + hybm(k)*ps
! 
!-----------------------------------------------------------------------
!!!  vertical level definitions in LASG dynamical core: p = pes*sigma + pt
!!!        interfaces   ply(k) = ps*sig (k) + pmtop
!!!        midpoints    ply(k) = ps*sigl(k) + pmtop
!!!---------------------------------------------------------------------
!!!(wanhui 2003.04.30)
!!!(wanhui 2003.10.23)  (std.atm. variables removed)

      real(r8) hyai(plevp)       ! ps0 component of hybrid coordinate - interfaces
      real(r8) hybi(plevp)       ! ps component of hybrid coordinate - interfaces
      real(r8) hyam(plev)        ! ps0 component of hybrid coordinate - midpoints
      real(r8) hybm(plev)        ! ps component of hybrid coordinate - midpoints

!!    real(r8) hybd(plev)        ! difference  in b (hybi) across layers
      real(r8) hypi(plevp)       ! reference pressures at interfaces
      real(r8) hypm(plev)        ! reference pressures at midpoints
!!    real(r8) hypd(plev)        ! reference pressure layer thickness

      real(r8) ps0         ! base state sfc pressure for level definitions
!!    real(r8) psr         ! reference surface pressure for linearization
!!    real(r8) prsfac      ! log pressure extrapolation factor (time, space independent)

!!    integer nprlev       ! number of pure pressure levels at top

      real(r8) :: pmtop              !
      real(r8) :: sig (plevp)        !
      real(r8) :: sigl(plev)         !  fm2003 VPAR variables
      real(r8) :: dsig(plev)         !

!!(wanhui 2003.10.23)
!!------------------------------------------------------------
!!    real(r8) :: tbb (plevstd)         !
!!    real(r8) :: hbb (plevstd)         !
!!    real(r8) :: cbb (plevstd)         !
!!    real(r8) :: dcbb(plevstd)         !  fm2003 std. atm.
!!    real(r8) :: p00, t00              !
!!    real(r8) :: psb (plond,plat)      !
!!    real(r8) :: tsb (plond,plat)      !
!!------------------------------------------------------------
!!(2003.10.23)(these variables are in module stdatm now)


!!      common /comhyb/ hyai ,hyam  ,hybi ,hybm
!!      common /comhyb/ hybd ,hypi ,hypm  ,hypd
!!      common /comhyb/ ps0         ,psr         ,prsfac      ,nprlev

      common /comhyb/ hyai ,hybi ,hyam ,hybm
      common /comhyb/ hypi ,hypm
      common /comhyb/ ps0  ,pmtop
      common /comhyb/ sig  ,sigl , dsig
!!    common /comhyb/ tbb  ,hbb  , cbb  ,dcbb  ,p00 ,t00 ,psb ,tsb    !!(wh 2003.10.23)
 
# 86 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/couple/c_coupler/coupling_chemistry_model_mod.F90" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/pdyn.h" 1
!!(wanhui 2003.11.05)
!!-------------------






      integer nprocessor
      parameter(nprocessor=4)            !by LPF
      integer,parameter :: nx = 130
      integer,parameter :: ny = 60/nprocessor+2
      integer,parameter :: nl = 26
      integer,parameter :: nz = 27








# 87 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/couple/c_coupler/coupling_chemistry_model_mod.F90" 2

        real*8 area(plon,plat)
        real*8 clondeg(plon,plat)
        real*8 clatdeg(plon,plat)
        integer :: mask(plon,plat)
        integer lat, lon, lchnk
        real(r8) :: spval = shr_const_spval          ! Special value for real msg data
        real                               :: min_lon, min_lat, max_lon, max_lat
        integer :: mid_v1d_grid_id
        integer, allocatable               :: local_grid_cell_indexes(:)
        integer :: ncol                  ! number of columns in current chunk
        integer :: i
        integer :: lats(pcols)           ! array of latitude indices
        integer :: lons(pcols)           ! array of longitude indices

        mask(:,:) = 0
        clatdeg(:,:) = spval
        clondeg(:,:) = spval
        do lat = 1, plat
        mask(1:nlon(lat),lat) = 1
        clatdeg(1:nlon(lat),lat) = latdeg(lat)
        clondeg(1:nlon(lat),lat) = londeg(1:nlon(lat),lat)
        end do

        min_lon = minval(londeg)
        max_lon = maxval(londeg)
        min_lat = minval(latdeg)
        max_lat = maxval(latdeg)

        grid_h2d_id = CCPL_register_H2D_grid_via_global_data(gamil_comp_id, "gamil_H2D_grid", "LON_LAT", "degrees", "cyclic", plon, plat, 0.0, 360.0, -90.0, 90.0, clondeg, clatdeg, mask,annotation="register gamil H2D grid")
        grid_v1d_id = CCPL_register_V1D_SIGMA_grid_via_model_data(gamil_comp_id, "gamil_V1D_grid", "Pa", pmtop, sig, "register gamil v1d grid")
        grid_3d_id = CCPL_register_MD_grid_via_multi_grids(gamil_comp_id, "gamil_3D_grid", grid_H2D_id, grid_V1D_id, annotation="register a gamil 3-d grid")
        call CCPL_register_mid_point_grid(grid_3d_id, grid_mid_3d_id, mid_v1d_grid_id,annotation = "register a mid point grid")

!register decomposition
       allocate(local_grid_cell_indexes(pcols*(endchunk-begchunk+1)))
       local_grid_cell_indexes=0
       do lchnk = begchunk,endchunk
          ncol = get_ncols_p(lchnk)
          call get_lon_all_p(lchnk, ncol, lons)
          call get_lat_all_p(lchnk, ncol, lats)
          do i=1,ncol
              local_grid_cell_indexes((lchnk-begchunk)*pcols+i)=(lats(i)-1)*(nx-2)+lons(i)
          end do
       end do
       decomp_id = CCPL_register_parallel_decomp("decomp_gamil_grid", grid_H2D_id, pcols*(endchunk-begchunk+1), local_grid_cell_indexes, "allocate for gamil grid")
       deallocate(local_grid_cell_indexes)

    end subroutine register_grids_decomps

    subroutine add_fld_for_coupling_chem(fld_name, units, long_name, num_lev, fld_id)
        implicit none
        character(len=*), intent(in) :: fld_name      
        character(len=*), intent(in) :: units 
        character(len=*), intent(in) :: long_name
        integer         , intent(in) :: num_lev
        integer                      :: i
        integer         , intent(out):: fld_id

        num_registered_flds_for_chem = num_registered_flds_for_chem + 1
        if (num_registered_flds_for_chem .gt. max_num_chem_flds) then
            call CCPL_report_error(gamil_comp_id, .false., "GAMIL register too many fields for coupling chemistry model", "too many fields")
        endif
        registered_flds_for_chem(num_registered_flds_for_chem)%name      = fld_name
        registered_flds_for_chem(num_registered_flds_for_chem)%num_lev   = num_lev
        registered_flds_for_chem(num_registered_flds_for_chem)%fld_buf(:,:,:) = 0.0

        if (num_lev .eq. 1) then
            fld_id = CCPL_register_field_instance(registered_flds_for_chem(num_registered_flds_for_chem)%fld_buf, fld_name, decomp_id, grid_h2d_id, 0, units, "register field instance of " // fld_name)
        else if (num_lev .eq. pver) then
            fld_id = CCPL_register_field_instance(registered_flds_for_chem(num_registered_flds_for_chem)%fld_buf, fld_name, decomp_id, grid_3d_id, 0, units, "register field instance of " // fld_name)
        else if (num_lev .eq. pverp) then
            fld_id = CCPL_register_field_instance(registered_flds_for_chem(num_registered_flds_for_chem)%fld_buf, fld_name, decomp_id, grid_mid_3d_id, 0, units, "register field instance of " // fld_name)
        else if (num_lev .eq. 1) then 
            fld_id = CCPL_register_field_instance(registered_flds_for_chem(num_registered_flds_for_chem)%fld_buf, fld_name, decomp_id, grid_h2d_id, 0, units, "register field instance of " // fld_name)
        else 
            call CCPL_report_error(gamil_comp_id, .false., "number of levels of fields for coupling chemistry model is not supported", "field level wrong")
        endif

    end subroutine add_fld_for_coupling_chem



    subroutine copy_fld_for_coupling_chem_3D(field_in, field_out, num_lev)
        implicit none
        real(r8), intent(in)         :: field_in(pcols,begchunk:endchunk,num_lev) 
        real(r8), intent(out)        :: field_out(pcols,begchunk:endchunk,num_lev) 
        integer , intent(in)         :: num_lev


        field_out(:,:,:) = field_in(:,:,:)
 
    end subroutine copy_fld_for_coupling_chem_3D



    subroutine copy_fld_for_coupling_chem_2D(field_in, field_out, num_lev)
        implicit none
        real(r8), intent(in)         :: field_in(pcols,begchunk:endchunk) 
        real(r8), intent(out)        :: field_out(pcols,begchunk:endchunk,num_lev) 
        integer , intent(in)         :: num_lev


        field_out(:,:,1) = field_in(:,:)
 
    end subroutine copy_fld_for_coupling_chem_2D



    subroutine copy_fld_for_coupling_chem_2D_lchnk(field_in, field_out, num_lev, lchnk)
        implicit none
        real(r8), intent(in)         :: field_in(pcols,num_lev) 
        real(r8), intent(out)        :: field_out(pcols,begchunk:endchunk,num_lev) 
        integer , intent(in)         :: num_lev
        integer , intent(in)         :: lchnk


        field_out(:,lchnk,:) = field_in(:,:)
 
    end subroutine copy_fld_for_coupling_chem_2D_lchnk



    subroutine copy_fld_for_coupling_chem_1D_lchnk(field_in, field_out, num_lev, lchnk)
        implicit none
        real(r8), intent(in)         :: field_in(pcols) 
        real(r8), intent(out)        :: field_out(pcols,begchunk:endchunk,num_lev) 
        integer , intent(in)         :: num_lev
        integer , intent(in)         :: lchnk


        field_out(:,lchnk,1) = field_in(:)
 
    end subroutine copy_fld_for_coupling_chem_1D_lchnk



    subroutine search_fld_index(fld_name, indx)
        implicit none
        character(len=*), intent(in) :: fld_name      
        integer,          intent(out) :: indx

        
        do indx = 1, num_registered_flds_for_chem
            if (registered_flds_for_chem(indx)%name == fld_name) then
                goto 200
            endif
        enddo 

200     if (indx .gt. num_registered_flds_for_chem) then
            call CCPL_report_error(gamil_comp_id, .false., "field has not been registerred when output it as a for coupling chemistry model")
        endif 

    end subroutine search_fld_index



    subroutine out_fld_for_coupling_chem_3D(fld_name, field_buf)
        implicit none
        character(len=*), intent(in) :: fld_name      
        real(r8), intent(in)         :: field_buf(:,:,:) ! Array containing field values
        integer                      :: indx

        
        call search_fld_index(fld_name, indx)
        call copy_fld_for_coupling_chem_3D(field_buf, registered_flds_for_chem(indx)%fld_buf, &
                                        registered_flds_for_chem(indx)%num_lev)

    end subroutine out_fld_for_coupling_chem_3D



    subroutine out_fld_for_coupling_chem_2D(fld_name, field_buf)
        implicit none
        character(len=*), intent(in) :: fld_name      
        real(r8), intent(in)         :: field_buf(:,:) ! Array containing field values
        integer                      :: indx

        
        call search_fld_index(fld_name, indx)
        if (registered_flds_for_chem(indx)%num_lev .ne. 1) then
            call CCPL_report_error(gamil_comp_id, .false., "number of levels of for 2D field has not been registerred correctly")
        endif
        call copy_fld_for_coupling_chem_2D(field_buf, registered_flds_for_chem(indx)%fld_buf, &
                                        registered_flds_for_chem(indx)%num_lev)

    end subroutine out_fld_for_coupling_chem_2D



    subroutine out_fld_for_coupling_chem_1D_lchnk(fld_name, field_buf, lchnk)
        implicit none
        character(len=*), intent(in) :: fld_name      
        real(r8), intent(in)         :: field_buf(:) ! Array containing field values
        integer, intent(in)          :: lchnk
        integer                      :: indx

        
        call search_fld_index(fld_name, indx)
        if (registered_flds_for_chem(indx)%num_lev .ne. 1) then
            call CCPL_report_error(gamil_comp_id, .false., "number of levels of for 2D field has not been registerred correctly")
        endif
        call copy_fld_for_coupling_chem_1D_lchnk(field_buf, registered_flds_for_chem(indx)%fld_buf, &
                                        registered_flds_for_chem(indx)%num_lev, lchnk)

    end subroutine out_fld_for_coupling_chem_1D_lchnk



    subroutine out_fld_for_coupling_chem_2D_lchnk(fld_name, field_buf, lchnk)
        implicit none
        character(len=*), intent(in) :: fld_name      
        real(r8), intent(in)         :: field_buf(:,:) ! Array containing field values
        integer, intent(in)          :: lchnk
        integer                      :: indx

        
        call search_fld_index(fld_name, indx)
        if (registered_flds_for_chem(indx)%num_lev .eq. 1) then
            call CCPL_report_error(gamil_comp_id, .false., "number of levels of for 3D field has not been registerred correctly")
        endif
        call copy_fld_for_coupling_chem_2D_lchnk(field_buf, registered_flds_for_chem(indx)%fld_buf, &
                                        registered_flds_for_chem(indx)%num_lev, lchnk)

    end subroutine out_fld_for_coupling_chem_2D_lchnk



    subroutine add_most_flds_for_coupling_chem
    implicit none
        allocate(PRECCON_array(pcols,begchunk:endchunk))
        allocate(PRECTOT_array(pcols,begchunk:endchunk))
        allocate(PRECSNO_array(pcols,begchunk:endchunk))
        allocate(RADLWG_array(pcols,begchunk:endchunk))
        allocate(RADSWG_array(pcols,begchunk:endchunk))
        allocate(FRLAKE_array(pcols,begchunk:endchunk))
        allocate(FRLANDIC_array(pcols,begchunk:endchunk))

        allocate(flds_id(10))

        call add_fld_for_coupling_chem('CLDF','fraction','Cloud fraction',pver,flds_id(1))
        call add_fld_for_coupling_chem('CMFMC','kg m-2 s-1','Moist convection mass flux',pverp,flds_id(2))
        call add_fld_for_coupling_chem('DQIDTMST','kg kg-1 s-1','ice tendency, mst proc',pver, flds_id(3))
        call add_fld_for_coupling_chem('DQLDTMST','kg kg-1 s-1','H2O tendency, mst proc',pver, flds_id(4))
        call add_fld_for_coupling_chem('DQVDTMST','kg kg-1 s-1','vapor tendency, mst proc',pver, flds_id(5))
        call add_fld_for_coupling_chem('DTRAIN','kg m-2 s-1','detrainment flux',pver, flds_id(6))
        call add_fld_for_coupling_chem('MOISTQ','g kg-1 day-1','tendency in sp. C17',pver, flds_id(7))
        call add_fld_for_coupling_chem('OPTDEP','1','visible optical depth',pver, flds_id(8))
        call add_fld_for_coupling_chem('SLP','Pa','sea level pressure',1, flds_id(9))
        call add_fld_for_coupling_chem('TS','K','surface temperature',1, flds_id(10))


    end subroutine add_most_flds_for_coupling_chem



    subroutine out_caculated_flds_for_coupling_chem()
        use comsrf, only: surface_state2d, srfflx_state2d, landfrac
        implicit none
        integer :: lchnk, ncols, i
        
        do lchnk = begchunk, endchunk
            ncols = get_ncols_p(lchnk)
            do i = 1, ncols
                PRECCON_array(i,lchnk) = surface_state2d(lchnk)%precc(i)*1000.
                PRECTOT_array(i,lchnk) = (surface_state2d(lchnk)%precl(i)+surface_state2d(lchnk)%precc(i))*1000.
                PRECSNO_array(i,lchnk) = (surface_state2d(lchnk)%precsc(i)+surface_state2d(lchnk)%precsl(i))*1000.
                RADLWG_array(i,lchnk)  = srfflx_state2d(lchnk)%lwup(i)-surface_state2d(lchnk)%flwds(i) 
                RADSWG_array(i,lchnk)  = surface_state2d(lchnk)%srfrad(i)-surface_state2d(lchnk)%flwds(i)
                !send2d_chunk(i,lchnk,atm_output_field_pbot)  = surface_state2d(lchnk)%pbot(i) ! Atmospheric state variable Pa
            end do
        end do
        call out_fld_for_coupling_chem('PRECCON',PRECCON_array)
        call out_fld_for_coupling_chem('PRECTOT',PRECTOT_array)
        call out_fld_for_coupling_chem('PRECSNO',PRECSNO_array)
        call out_fld_for_coupling_chem('RADLWG',RADLWG_array)
        call out_fld_for_coupling_chem('RADSWG',RADSWG_array)
        call out_fld_for_coupling_chem('FRLAND',landfrac)

    end subroutine out_caculated_flds_for_coupling_chem

    subroutine register_component_coupling_configuration(comm, comp_name, &
               export_interface_id, local_comp_id, time_step)
       use pmgrid
       use ppgrid
       use phys_grid
       use rgrid,          only: nlon                                                  ! reduced grid
       use commap
       use dycore, only: dycore_is
       use shr_const_mod,  only: shr_const_spval
       use CCPL_interface_mod
       implicit none


# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/comhyb.h" 1
!----------------------------------------------------------------------- 
! 
! Purpose: Hybrid level definitions: p = a*p0 + b*ps
!          interfaces   p(k) = hyai(k)*ps0 + hybi(k)*ps
!          midpoints    p(k) = hyam(k)*ps0 + hybm(k)*ps
! 
!-----------------------------------------------------------------------
!!!  vertical level definitions in LASG dynamical core: p = pes*sigma + pt
!!!        interfaces   ply(k) = ps*sig (k) + pmtop
!!!        midpoints    ply(k) = ps*sigl(k) + pmtop
!!!---------------------------------------------------------------------
!!!(wanhui 2003.04.30)
!!!(wanhui 2003.10.23)  (std.atm. variables removed)

      real(r8) hyai(plevp)       ! ps0 component of hybrid coordinate - interfaces
      real(r8) hybi(plevp)       ! ps component of hybrid coordinate - interfaces
      real(r8) hyam(plev)        ! ps0 component of hybrid coordinate - midpoints
      real(r8) hybm(plev)        ! ps component of hybrid coordinate - midpoints

!!    real(r8) hybd(plev)        ! difference  in b (hybi) across layers
      real(r8) hypi(plevp)       ! reference pressures at interfaces
      real(r8) hypm(plev)        ! reference pressures at midpoints
!!    real(r8) hypd(plev)        ! reference pressure layer thickness

      real(r8) ps0         ! base state sfc pressure for level definitions
!!    real(r8) psr         ! reference surface pressure for linearization
!!    real(r8) prsfac      ! log pressure extrapolation factor (time, space independent)

!!    integer nprlev       ! number of pure pressure levels at top

      real(r8) :: pmtop              !
      real(r8) :: sig (plevp)        !
      real(r8) :: sigl(plev)         !  fm2003 VPAR variables
      real(r8) :: dsig(plev)         !

!!(wanhui 2003.10.23)
!!------------------------------------------------------------
!!    real(r8) :: tbb (plevstd)         !
!!    real(r8) :: hbb (plevstd)         !
!!    real(r8) :: cbb (plevstd)         !
!!    real(r8) :: dcbb(plevstd)         !  fm2003 std. atm.
!!    real(r8) :: p00, t00              !
!!    real(r8) :: psb (plond,plat)      !
!!    real(r8) :: tsb (plond,plat)      !
!!------------------------------------------------------------
!!(2003.10.23)(these variables are in module stdatm now)


!!      common /comhyb/ hyai ,hyam  ,hybi ,hybm
!!      common /comhyb/ hybd ,hypi ,hypm  ,hypd
!!      common /comhyb/ ps0         ,psr         ,prsfac      ,nprlev

      common /comhyb/ hyai ,hybi ,hyam ,hybm
      common /comhyb/ hypi ,hypm
      common /comhyb/ ps0  ,pmtop
      common /comhyb/ sig  ,sigl , dsig
!!    common /comhyb/ tbb  ,hbb  , cbb  ,dcbb  ,p00 ,t00 ,psb ,tsb    !!(wh 2003.10.23)
 
# 381 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/couple/c_coupler/coupling_chemistry_model_mod.F90" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/pdyn.h" 1
!!(wanhui 2003.11.05)
!!-------------------






      integer nprocessor
      parameter(nprocessor=4)            !by LPF
      integer,parameter :: nx = 130
      integer,parameter :: ny = 60/nprocessor+2
      integer,parameter :: nl = 26
      integer,parameter :: nz = 27








# 382 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/couple/c_coupler/coupling_chemistry_model_mod.F90" 2

       character(len=*), intent(in)       :: comp_name
       integer, intent(inout)             :: comm
       integer, intent(in)                :: local_comp_id
       integer, intent(out)               :: export_interface_id
       integer, intent(in)                :: time_step
       character*1024                     :: annotation
       integer                            :: timer1_id, timer2_id
       integer                            :: num_proc, proc_id
       integer, allocatable               :: timers_id(:), fields_id(:)
       logical                            :: interface_status

       proc_id = CCPL_get_current_process_id_in_component(gamil_comp_id)

       call CCPL_set_time_step(gamil_comp_id, time_step)

       annotation = "component "//comp_name//" start registration"

       call register_grids_decomps
       call add_most_flds_for_coupling_chem
       call CCPL_set_3D_grid_dynamic_surface_field(grid_3d_id, fields_id(1), "set bottom field of a 3-D grid")

       allocate(timers_id(10),fields_id(10))
       timer1_id = CCPL_define_single_timer(gamil_comp_id, "steps", 1, 0, 0, annotation="define a single timer for comp_id_gamil")
       timer2_id = CCPL_define_single_timer(gamil_comp_id, "seconds", 2400, 0, 0, annotation="define a single timer for comp_id_gamil")

       export_interface_id = CCPL_register_export_interface("send_data_to_GIGC", 10, fields_id, timer1_id, "gamil_component_send", annotation="register interface for sending data to GIGC")
       call CCPL_end_coupling_configuration(gamil_comp_id, annotation = "component "//comp_name//" end registration")
       deallocate(timers_id, fields_id)

    end subroutine register_component_coupling_configuration

end module coupling_chemistry_model_mod
