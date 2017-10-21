!***************************************************************
!  This is a source file of GAMIL, which registers all parallel 
!  decompositions into C-Coupler library. This file was initially 
!  finished by Dr. Li Liu. If you have any problem, please 
!  contact Dr. Li Liu via liuli-cess@tsinghua.edu.cn
!***************************************************************


module component_mgt


      USE CMN_SIZE_MOD
      USE GIGC_State_Met_Mod, ONLY : MetState
      USE CCPL_interface_mod

      REAL*8, allocatable   :: OPTDEP_CCPL(:,:,:)

    type, private :: comp_states_fluxes
        real, allocatable :: CMFMC(:,:,:), U10M(:,:), CLDF(:,:,:), OPTDEP(:,:,:)
        integer :: comp_id
        integer, allocatable :: local_grid_cell_indexes(:)
        integer :: time_step
        logical :: first
        integer :: grid_V1D_id, grid_H2D_id
    end type comp_states_fluxes

    type(comp_states_fluxes), public :: comps_states_fluxes(100)
       integer, public                    :: gigc_decomp_id, gigc_grid_h2d_id
       integer, public                    :: gigc_grid_v1d_id, gigc_grid_3d_id
       integer, public                    :: gigc_grid_mid_3d_id
       integer, public                    :: gigc_comp_id

   contains

       subroutine register_gigc_component(comm)
           integer, intent(inout) :: comm
           gigc_comp_id = CCPL_register_component(-1, "GIGC", "atm_chem", comm, .true., "register atm_chem model GIGC")
       end subroutine register_gigc_component

    subroutine register_component_coupling_configuration(comm, comp_name, comp_id, import_interface_id, local_comp_id, time_step,mask,State_Met)

      use CMN_SIZE_MOD
      use GIGC_State_Met_Mod, only : MetState
      use Grid_Mod, only : GET_XMID, GET_YMID, GET_XEDGE, GET_YEDGE, GET_AREA_M2
      use PRESSURE_MOD, only : GET_AP, GET_BP, GET_PEDGE
      USE DAO_MOD,            ONLY : IS_LAND
      use CCPL_interface_mod

       implicit none

       character(len=*), intent(in)       :: comp_name
       integer, intent(inout)             :: comm
       integer, intent(in)                :: local_comp_id,comp_id
       integer, intent(out)               :: import_interface_id
       integer, intent(in)                :: time_step
       integer, intent(in)                :: mask(:,:)
       type(MetState), intent(inout)      :: State_Met
       character*1024                     :: annotation, local_comp_full_name, remote_comp_full_name
       integer                            :: timer1_id, timer2_id
       integer                            :: grid_H2D_size, decomp_size, num_proc, proc_id, grid_V1D_size
       integer, allocatable               :: timers_id(:), fields_id(:), local_grid_cell_indexes(:)
       logical                            :: interface_status
       integer                            :: I,J,L,n,X1,Y1,X2,Y2,num,k
       real*8                             :: min_lon, min_lat, max_lon, max_lat

       !-----------------local variables------------------------------------------------
       real*8 AREA_M2(IIPAR,JJPAR)
       real*8 XMID(IIPAR,JJPAR)
       real*8 YMID(IIPAR,JJPAR)
       real*8 XEDGE(4,IIPAR,JJPAR)
       real*8 YEDGE(4,IIPAR,JJPAR)
       !integer MASK(IIPAR,JJPAR)
       real*8 SIGE_GCAP(LLPAR+1)  
       real*8 AP(LLPAR+1)
       real*8 BP(LLPAR+1)
       real*8 PEDGE(LLPAR+1)

       !-----------------fields to be registered-----------------------------------------
       !-----------------get proc id----------------------------------------------------
       proc_id = CCPL_get_current_process_id_in_component(gigc_comp_id)
       !set time step
       call CCPL_set_time_step(gigc_comp_id, time_step)

       !----------------------------------------------------------------------------
       comps_states_fluxes(local_comp_id)%time_step = time_step
       comps_states_fluxes(local_comp_id)%first = .true.
       !----------------------------------------------------------------------------
       !register grids
       do J = 1, JJPAR
       do I = 1, IIPAR
          XMID(I,J) = GET_XMID(I,J,1)+180.0
          YMID(I,J) = GET_YMID(I,J,1)
          XEDGE(1,I,J) = GET_XEDGE(I,J,1)+180.0
          XEDGE(2,I,J) = GET_XEDGE(I+1,J,1)+180.0
          XEDGE(3,I,J) = GET_XEDGE(I,J+1,1)+180.0
          XEDGE(4,I,J) = GET_XEDGE(I+1,J+1,1)+180.0
          YEDGE(1,I,J) = GET_YEDGE(I,J,1)
          YEDGE(2,I,J) = GET_YEDGE(I+1,J,1)
          YEDGE(3,I,J) = GET_YEDGE(I,J+1,1)
          YEDGE(4,I,J) = GET_YEDGE(I+1,J+1,1)
          AREA_M2(I,J) = GET_AREA_M2(I,J,1)

       end do
       end do
       min_lon = 0.0
       min_lat = -90.0
       max_lon = 360.0
       max_lat = 90.0
       do L = 1,LLPAR+1
        AP(L) = GET_AP(L)
        BP(L) = GET_BP(L)
        PEDGE(L) = GET_PEDGE(1,1,L)
       end do
     !-----------------------------------------------------------------------------------------

       annotation = "component "//comp_name//" start registration"

       !grid_H2D_id = CCPL_register_H2D_grid_via_global_data(comp_id, "gigc_H2D_grid", "LON_LAT", "degrees", "cyclic", IIPAR, JJPAR, min_lon, max_lon, min_lat, max_lat, XMID, YMID, mask, AREA_M2, XEDGE, YEDGE, "register GIGC H2D grid")
       gigc_grid_h2d_id = CCPL_register_H2D_grid_via_global_data(gigc_comp_id, "gigc_H2D_grid", "LON_LAT", "degrees", "cyclic", IIPAR, JJPAR, min_lon, max_lon, min_lat, max_lat, XMID, YMID, mask)
       gigc_grid_v1d_id = CCPL_register_V1D_HYBRID_grid_via_model_data(gigc_comp_id, "gigc_V1D_grid", "LON_LAT", PTOP, AP, BP, "register gigc v1d grid")
       grid_H2D_size = CCPL_get_grid_size(gigc_grid_h2d_id, "get the size of H2D grid")
       grid_V1D_size = CCPL_get_grid_size(gigc_grid_v1d_id, "get the size of V1D grid")
       num_proc = CCPL_get_num_process_in_component(comp_id, "get number of processes")
       gigc_grid_3d_id = CCPL_register_MD_grid_via_multi_grids(gigc_comp_id, "gigc_3D_grid", gigc_grid_h2d_id, gigc_grid_v1d_id, annotation="register a gigc 3-d grid")
       !----------------------------------------------------------------------------------------------------------
       !register decomposition
       allocate(local_grid_cell_indexes(IIPAR*JJPAR))
       allocate(OPTDEP_CCPL(IIPAR, JJPAR, LLPAR))
       num=0
       do j=1,JJPAR
       do i=1,IIPAR
            num=num+1
            local_grid_cell_indexes(num) = i+(j-1)*IIPAR
       enddo
       enddo

       gigc_decomp_id = CCPL_register_parallel_decomp("decomp_gigc_grid", gigc_grid_h2d_id, num, local_grid_cell_indexes, "allocate for gigc grid")

       !register phys static variables
       fields_id(1) = CCPL_register_field_instance(State_Met%SLP, "SLP", gigc_decomp_id, gigc_grid_h2d_id, 1, "Pa", "register field instance of SLP")
       fields_id(2) = CCPL_register_field_instance(State_Met%CLDFRC, "CLDFRC", gigc_decomp_id, gigc_grid_h2d_id, 1, "fraction", "register field instance of CLDFRC")
       fields_id(3) = CCPL_register_field_instance(State_Met%TS, "TS", gigc_decomp_id, gigc_grid_h2d_id, 1, "K", "register field instance of TS")
       fields_id(4) = CCPL_register_field_instance(State_Met%CMFMC, "CMFMC", gigc_decomp_id, gigc_grid_3d_id, 1, "kg m-2 s-1", "register field instance of CMFMC")
       fields_id(5) = CCPL_register_field_instance(State_Met%DQIDTMST, "DQIDTMST", gigc_decomp_id, gigc_grid_3d_id, 1, "kg kg-1 s-1", "register field instance of DQIDTMST")
       fields_id(6) = CCPL_register_field_instance(State_Met%DQLDTMST, "DQLDTMST", gigc_decomp_id, gigc_grid_3d_id, 1, "kg kg-1 s-1", "register field instance of DQLDTMST")
       fields_id(7) = CCPL_register_field_instance(State_Met%DQVDTMST, "DQVDTMST", gigc_decomp_id, gigc_grid_3d_id, 1, "kg kg-1 s-1", "register field instance of DQVDTMST")
       fields_id(8) = CCPL_register_field_instance(State_Met%DTRAIN, "DTRAIN", gigc_decomp_id, gigc_grid_3d_id, 1, "kg m-2 s-1", "register field instance of DTRAIN")
       fields_id(9) = CCPL_register_field_instance(State_Met%MOISTQ, "MOISTQ", gigc_decomp_id, gigc_grid_3d_id, 1, "g kg-1 day-1", "register field instance of MOISTQ")
       fields_id(10) = CCPL_register_field_instance(OPTDEP_CCPL, "OPTDEP", gigc_decomp_id, gigc_grid_3d_id, 1, "1", "register field instance of OPTDEP")

       call transform_CCPL_arrays(State_Met)

       call CCPL_set_3D_grid_dynamic_surface_field(gigc_grid_3d_id, fields_id(1), "set bottom field of a 3-D grid")
       !register interface
       allocate(timers_id(10),fields_id(10))
       timer1_id = CCPL_define_single_timer(comp_id, "steps", 1, 0, 0, annotation="define a single timer for comp_id_gamil")
       timer2_id = CCPL_define_single_timer(comp_id, "seconds", 2400, 0, 0, annotation="define a single timer for comp_id_gamil")


       import_interface_id = CCPL_register_import_interface("receive_data_from_GAMIL", 10, fields_id, timer1_id, 0, "gigc$receive_data_from_GAMIL", "register interface for receiving data from GAMIL")
       call CCPL_end_coupling_configuration(comp_id, annotation = "component "//comp_name//" end registration")
       deallocate(timers_id, fields_id)

       end subroutine register_component_coupling_configuration


!    subroutine run_component(local_id, time_length)
!      use CMN_SIZE_MOD
!      use GIGC_State_Met_Mod, only : MetState
!      use Global_Grid_Mod
!      use PRESSURE_MOD
!      use CCPL_interface_mod
!        implicit none
!        integer, intent(in) :: local_id, time_length
!        integer i,j,k, grid_H2D_size, num_proc, decomp_size, grid_V1D_size
!        logical interface_status
!
!        !grid_H2D_size = CCPL_get_grid_size(comps_states_fluxes(local_id)%grid_H2D_id, "get the size of H2D grid")
!        !grid_V1D_size = CCPL_get_grid_size(comps_states_fluxes(local_id)%grid_V1D_id, "get the size of V1D grid")
!        !num_proc = CCPL_get_num_process_in_component(comps_states_fluxes(local_id)%comp_id, "get number of processes")
!        !decomp_size = grid_H2D_size / num_proc
!        if (comps_states_fluxes(local_id)%first) then
!            interface_status = CCPL_execute_interface_using_name(gigc_comp_id, "receive_data_from_GAMIL", .true.)
!        end if
!        
!        do i=1,time_length/comps_states_fluxes(local_id)%time_step
!            interface_status = CCPL_execute_interface_using_name(gigc_comp_id, "receive_data_from_GAMIL", .false.)
!            call CCPL_advance_time(gigc_comp_id)
!            call CCPL_do_restart_write(gigc_comp_id, .false.)
!        end do
!        comps_states_fluxes(local_id)%first = .false.
!        end subroutine run_component

      subroutine transform_CCPL_arrays(State_Met)
      USE GIGC_State_Met_Mod, ONLY : MetState
      implicit none
      TYPE(MetState), INTENT(INOUT) :: State_Met
      integer       :: i, j, k

      do k = 1, LLPAR
      do j = 1, JJPAR
      do i = 1, IIPAR
!         State_Met%CLDF(k,i,j) = CLDF_CCPL(i,j,k)
         State_Met%OPTDEP(k,i,j) = OPTDEP_CCPL(i,j,k)
      enddo
      enddo
      enddo

      end subroutine transform_CCPL_arrays
    end module component_mgt
