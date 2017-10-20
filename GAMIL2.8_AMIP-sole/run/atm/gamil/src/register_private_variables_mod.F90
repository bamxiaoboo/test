!***************************************************************
!  This is a source file of GAMIL, which registers all variables
!  into C-Coupler library for I/O. This file was initially 
!  finished by Dr. Li Liu. If you have any problem, please 
!  contact Dr. Li Liu via liuli-cess@tsinghua.edu.cn
!***************************************************************


#include <misc.h>
#include <params.h>


module register_private_variables_mod

    use shr_kind_mod, only: r8 => shr_kind_r8
    use ppgrid
    use phys_grid,    only: read_chunk_from_field, write_field_from_chunk, get_ncols_p
    use pmgrid,       only: masterproc
    use prognostics,  only: ptimelevels, n3, n3m2
    use buffer
    use radae,        only: abstot_3d, absnxt_3d, emstot_3d, initialize_radbuffer
    use comsrf
    use ioFileMod
    use phys_buffer
    use CCPL_interface_mod

    implicit none
#include <pdyn.h> 
#include <comfm2.h> 

    !
    ! Public interfaces
    !
    public  register_static_variables
    private register_dyn_variables

CONTAINS

    

    subroutine register_dyn_variables
       use prognostics
       use comfm1
       use pmgrid, only: beglatex,beglatexdyn,endlatexdyn
       implicit none

!       call c_coupler_register_model_data(pes,"gamil_2D_decomp_dyn","gamil_pes",.true.)
!       call c_coupler_register_model_data(t,"gamil_2D_decomp_dyn","gamil_t",.true.)
       !call c_coupler_add_field_for_perturbing_roundoff_errors(pes)
!       call c_coupler_add_field_for_perturbing_roundoff_errors(t)

    end subroutine register_dyn_variables



    subroutine register_static_variables

       implicit none

       call register_dyn_variables

    end subroutine register_static_variables


end module register_private_variables_mod
