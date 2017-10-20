!***************************************************************
!  This is a source file of GAMIL, which registers all parallel 
!  decompositions into C-Coupler library. This file was initially 
!  finished by Dr. Li Liu. If you have any problem, please 
!  contact Dr. Li Liu via liuli-cess@tsinghua.edu.cn
!***************************************************************


module component_mgt
    type, private :: comp_states_fluxes
        real, allocatable :: CMFMC(:,:,:), U10M(:,:), CLDF(:,:), OPTDEP(:,:)
        integer :: comp_id
        integer, allocatable :: local_grid_cell_indexes(:)
        integer :: time_step
        logical :: first
        integer :: grid_V1D_id, grid_H2D_id
    end type comp_states_fluxes

    type(comp_states_fluxes), public :: comps_states_fluxes(100)

contains

    subroutine register_component_coupling_configuration(comm, comp_name, comp_id, &
               import_interface_id, export_interface_id, local_comp_id, time_step)
       !use mpi_gamil
       use pmgrid
       use phys_grid
       use rgrid,          only: nlon                                                  ! reduced grid
       use commap,         only: latdeg, londeg
       use dycore, only: dycore_is
       use shr_const_mod,  only: shr_const_spval
       use CCPL_interface_mod
       implicit none

#include <comhyb.h>

       integer,allocatable :: decomp_cell_indexes(:)
       character(len=*), intent(in)       :: comp_name
       integer, intent(inout)             :: comm
       integer, intent(in)                :: local_comp_id,comp_id
       integer, intent(out)               :: import_interface_id, export_interface_id
       integer, intent(in)                :: time_step
       character*1024                     :: annotation, local_comp_full_name, remote_comp_full_name
       integer                            :: grid_H2D_id, decomp_id, timer1_id, timer2_id, decomp_H2D_id_local
       integer                            :: grid_H2D_size, decomp_size, num_proc, proc_id, grid_3D_id, grid_V1D_size, grid_V1D_id, grid_H2D_id_local
       integer, allocatable               :: timers_id(:), fields_id(:), local_grid_cell_indexes(:)
       logical                            :: interface_status
       real                               :: min_lon, min_lat, max_lon, max_lat
       integer                            :: field_id_cmfmc,field_id_u10m,field_id_cldf,field_id_optdep

       integer :: lchnk         ! indices
       integer :: ncol                  ! number of columns in current chunk

       !-----------------local variables------------------------------------------------
       real area(plon,plat)
       real clondeg(plon,plat)
       real clatdeg(plon,plat)
       real ew_vert(4,plon,plat)
       real ns_vert(4,plon,plat)
       integer :: mask(plon,plat)
       real(r8) del_theta
       real(r8) del_phi
       real(r8) pie
       real(r8) degtorad
       integer lat, lon, i, j, n     ! loop indices
       integer startpoint            ! starting value for grid numbering scheme
       integer bufsize

       !-----------------fields to be registered-----------------------------------------
       real(r8), allocatable :: CMFMC(:,:,:) 
       real(r8), allocatable :: U10M(:,:) 

       real(r8), allocatable :: CLDF(:,:,:)
       real(r8), allocatable :: OPTDEP(:,:,:)

       real(r8) :: spval = shr_const_spval          ! Special value for real msg data
       bufsize=0
       do j=1,plat
          bufsize = bufsize + nlon(j)
       end do
       allocate(decomp_cell_indexes(bufsize))
       n = 0
       startpoint = 0
       do j=1,plat
          do i=1,nlon(j)
             if(get_chunk_owner_p(i,j) .eq. iam) then
                n=n+1
                decomp_cell_indexes(n) = startpoint + i
             end if
          enddo
          startpoint = startpoint + nlon(j)
       enddo

       pie = acos(-1.)
       degtorad = pie/180.0
       !------------------------------------------------------------------------------------------
! Send vertices of each grid point
! Verticies are ordered as follows:
! 1=lower left, 2 = upper left, 3 = upper right, 4 = lower right
!
       ns_vert(:,:,:) = spval
       ew_vert(:,:,:) = spval
       
! Longitude vertices
!
       do lat = 1, plat
         ew_vert(1,1,lat)             = (londeg(1,lat) - 360.0 + londeg(nlon(lat),lat))*0.5
         ew_vert(1,2:nlon(lat),lat)   = (londeg(1:nlon(lat)-1,lat) + &
                                         londeg(2:nlon(lat),lat))*0.5
         ew_vert(2,:nlon(lat),lat)    = ew_vert(1,:nlon(lat),lat)  ! Copy lowleft corner to upleft
         ew_vert(3,:nlon(lat)-1,lat)  = ew_vert(1,2:nlon(lat),lat)
         ew_vert(3,nlon(lat),lat)     = (londeg(nlon(lat),lat) + (360.0 + londeg(1,lat)))*0.5
         ew_vert(4,:nlon(lat),lat)    = ew_vert(3,:nlon(lat),lat)  ! Copy lowright corner to upright
       end do
!
! Latitude
!
       if ( dycore_is('LR') )then
         ns_vert(1,:nlon(1),1)         = -90.0 + (latdeg(1) - latdeg(2))*0.5 
         ns_vert(2,:nlon(plat),plat)   =  90.0 + (latdeg(plat) - latdeg(plat-1))*0.5
       else
         ns_vert(1,:nlon(1),1)         = -90.0
         ns_vert(2,:nlon(plat),plat)   =  90.0
       end if
       ns_vert(4,:nlon(1),1)       = ns_vert(1,nlon(1),1)        ! Copy lower left to lower right
       ns_vert(3,:nlon(plat),plat) = ns_vert(2,nlon(plat),plat)  ! Copy up left to up right
       do lat = 2, plat
         ns_vert(1,:nlon(lat),lat) = (latdeg(lat) + latdeg(lat-1) )*0.5
         ns_vert(4,:nlon(lat),lat) = ns_vert(1,:nlon(lat),lat)
       end do
       do lat = 1, plat-1
         ns_vert(2,:nlon(lat),lat) = (latdeg(lat) + latdeg(lat+1) )*0.5
         ns_vert(3,:nlon(lat),lat) = ns_vert(2,:nlon(lat),lat)
       end do

       !get area of grid cells
       
       area(:,:) = 0.0
       do lat = 1,plat
       do lon = 1,nlon(lat)
           del_phi = sin( ns_vert(2,lon,lat)*degtorad ) - sin( ns_vert(1,lon,lat)*degtorad )
           del_theta = ( ew_vert(4,lon,lat) - ew_vert(1,lon,lat) )*degtorad
           area(lon,lat) = del_theta*del_phi
       end do
       end do
! If grid has a pole point (as in Lin-Rood dynamics
!
      if ( dycore_is('LR') )then
         lat = 1
!         mask(2:nlon(lat),lat) = 0   ! Only active one point on pole
         do lon = 1, nlon(lat)
           del_phi = -sin( latdeg(lat)*degtorad ) + sin( ns_vert(2,lon,lat)*degtorad )
           del_theta = ( ew_vert(4,lon,lat) - ew_vert(1,lon,lat) )*degtorad
           area(lon,lat)  = del_theta*del_phi
         end do
         lat = plat
!         mask(2:nlon(lat),lat) = 0   ! Only active one point on pole
         do lon = 1, nlon(lat)
           del_phi =  sin( latdeg(lat)*degtorad ) - sin( ns_vert(1,lon,lat)*degtorad )
           del_theta = ( ew_vert(4,lon,lat) - ew_vert(1,lon,lat) )*degtorad
           area(lon,lat)  = del_theta*del_phi
         end do
       end if
       if ( abs(sum(area) - 4.0*pie) > 1.e-12 )then
         write (6,*) 'CCSM_MSG_SENDGRID: sum of areas on globe does not = 4*pi'
         write (6,*) ' sum of areas = ', sum(area)
         call endrun
       end if

       mask(:,:) = 0
       clatdeg(:,:) = spval
       clondeg(:,:) = spval
       do lat = 1, plat
         mask(1:nlon(lat),lat)    = 1     ! Active cells
         clatdeg(1:nlon(lat),lat) = latdeg(lat) ! Put latitude in 2D array
         clondeg(1:nlon(lat),lat) = londeg(1:nlon(lat),lat)
       end do

       !------------------------------------------------------------------------------------------

       proc_id = CCPL_get_current_process_id_in_component(comp_id)

!set time step
       call CCPL_set_time_step(comp_id, time_step)

       !-----------------------------------------------------------------------------------
       comps_states_fluxes(local_comp_id)%comp_id = comp_id
       comps_states_fluxes(local_comp_id)%time_step = time_step
       comps_states_fluxes(local_comp_id)%first = .true.
       !-----------------------------------------------------------------------------------

       !register grids

       min_lon = minval(londeg)
       max_lon = maxval(londeg)
       min_lat = minval(latdeg)
       max_lat = maxval(latdeg)

       annotation = "component "//comp_name//" start registration"

       grid_H2D_id = CCPL_register_H2D_grid_via_global_data(comp_id, "gamil_H2D_grid", "LON_LAT", "degrees", "cyclic", plon, plat, 0.0, 360.0, -90.0, 90.0, clondeg, clatdeg, mask, area, ew_vert, ns_vert, annotation="register gamil H2D grid")
       grid_V1D_id = CCPL_register_V1D_SIGMA_grid_via_model_data(comp_id, "gamil_V1D_grid", "Pa", pmtop, sig, "register gamil v1d grid")
       grid_H2D_size = CCPL_get_grid_size(grid_H2D_id, "get the size of H2D grid")
       grid_V1D_size = CCPL_get_grid_size(grid_V1D_id, "get the size of V1D grid")
       num_proc = CCPL_get_num_process_in_component(comp_id, "get number of processes")

       grid_3D_id = CCPL_register_MD_grid_via_multi_grids(comp_id, "gamil_3D_grid", grid_H2D_id, grid_V1D_id, annotation="register a gamil 3-d grid")

       !--------------------------------------------------------------------------------------------
       comps_states_fluxes(local_comp_id)%grid_H2D_id = grid_H2D_id
       comps_states_fluxes(local_comp_id)%grid_V1D_id = grid_V1D_id
       !--------------------------------------------------------------------------------------------
!register decomposition
       decomp_size = grid_H2D_size / num_proc
       allocate(comps_states_fluxes(local_comp_id)%local_grid_cell_indexes(decomp_size))

       do i=1, decomp_size
       comps_states_fluxes(local_comp_id)%local_grid_cell_indexes(i) = i+decomp_size*proc_id
       end do

       decomp_id = CCPL_register_parallel_decomp("decomp_gamil_grid", grid_H2D_id, decomp_size, comps_states_fluxes(local_comp_id)%local_grid_cell_indexes, "allocate for gamil grid")
       !initialize_phys_io_arrays
       allocate(comps_states_fluxes(local_comp_id)%CMFMC(pcols,begchunk:endchunk,pver)) 
       allocate(comps_states_fluxes(local_comp_id)%U10M(pcols,begchunk:endchunk)) 
       !allocate(comps_states_fluxes(local_comp_id)%CLDF(pcols,begchunk:endchunk,pver)) 
       !allocate(comps_states_fluxes(local_comp_id)%OPTDEP(pcols,begchunk:endchunk,pver)) 

       !register phys static variables
       field_id_cmfmc = CCPL_register_field_instance(comps_states_fluxes(local_comp_id)%CMFMC, "CMFMC", decomp_id, grid_3D_ID, 0, "kg m-2 s-1", "register field instance of CMFMC")
       field_id_u10m = CCPL_register_field_instance(comps_states_fluxes(local_comp_id)%U10M, "U10M", decomp_id, grid_H2D_ID, 0, "m s-1", "register field instance of U10M")
       field_id_cldf = CCPL_register_field_instance(comps_states_fluxes(local_comp_id)%CLDF, "CLDF", decomp_id, grid_3D_ID, 0, "kg m-2 s-1", "register field instance of CLDF")
       field_id_optdep = CCPL_register_field_instance(comps_states_fluxes(local_comp_id)%OPTDEP, "OPTDEP", decomp_id, grid_3D_ID, 0, "kg m-2 s-1", "register field instance of OPTDEP")
       call CCPL_set_3D_grid_dynamic_surface_field(grid_3D_id, field_id_cmfmc, "set bottom field of a 3-D grid")

       !register interface
       allocate(timers_id(10),fields_id(10))
       timer1_id = CCPL_define_single_timer(comp_id, "steps", 1, 0, 0, annotation="define a single timer for comp_id_gamil")
       timer2_id = CCPL_define_single_timer(comp_id, "seconds", 2400, 0, 0, annotation="define a single timer for comp_id_gamil")

       fields_id(1) = field_id_CMFMC
       fields_id(2) = field_id_U10M
       export_interface_id = CCPL_register_export_interface("send_data_to_GIGC", 2, fields_id, timer1_id, "gamil_component_send", annotation="register interface for sending data to GIGC")

       fields_id(1) = field_id_CLDF
       fields_id(2) = field_id_OPTDEP
       import_interface_id = CCPL_register_import_interface("receive_data_from_GIGC", 2, fields_id, timer1_id, 0, annotation="register interface for receiving data from GIGC")
       annotation = "component "//comp_name//" end registration"
       call CCPL_end_coupling_configuration(comp_id, annotation)
       deallocate(timers_id, fields_id)

    end subroutine register_component_coupling_configuration


    subroutine run_component(local_id, time_length, base_value)
        use CCPL_interface_mod
        implicit none
        integer, intent(in) :: local_id, time_length, base_value
        integer i,j,k, grid_H2D_size, num_proc, decomp_size, grid_V1D_size
        logical interface_status

        grid_H2D_size = CCPL_get_grid_size(comps_states_fluxes(local_id)%grid_H2D_id, "get the size of H2D grid")
        grid_V1D_size = CCPL_get_grid_size(comps_states_fluxes(local_id)%grid_V1D_id, "get the size of V1D grid")
        num_proc = CCPL_get_num_process_in_component(comps_states_fluxes(local_id)%comp_id, "get number of processes")
        decomp_size = grid_H2D_size / num_proc

        do i=1,time_length/comps_states_fluxes(local_id)%time_step
            interface_status = CCPL_execute_interface_using_name(comps_states_fluxes(local_id)%comp_id, "send_data_to_CPL", .false.)
            interface_status = CCPL_execute_interface_using_name(comps_states_fluxes(local_id)%comp_id, "receive_data_from_CPL", .false.)
            call CCPL_advance_time(comps_states_fluxes(local_id)%comp_id)
            call CCPL_do_restart_write(comps_states_fluxes(local_id)%comp_id, .false.)
        end do
        comps_states_fluxes(local_id)%first = .false.
    end subroutine run_component
end module component_mgt
