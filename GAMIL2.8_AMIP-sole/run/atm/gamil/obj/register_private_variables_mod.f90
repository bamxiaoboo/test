# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/couple/c_coupler/register_private_variables_mod.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/couple/c_coupler/register_private_variables_mod.F90"
!***************************************************************
!  This is a source file of GAMIL, which registers all variables
!  into C-Coupler library for I/O. This file was initially 
!  finished by Dr. Li Liu. If you have any problem, please 
!  contact Dr. Li Liu via liuli-cess@tsinghua.edu.cn
!***************************************************************



# 1 "./misc.h" 1
# 10 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/couple/c_coupler/register_private_variables_mod.F90" 2

# 1 "./params.h" 1
# 11 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/couple/c_coupler/register_private_variables_mod.F90" 2


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

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/pdyn.h" 1
!!(wanhui 2003.11.05)
!!-------------------






      integer nprocessor
      parameter(nprocessor=20)            !by LPF
      integer,parameter :: nx = 130
      integer,parameter :: ny = 60/nprocessor+2
      integer,parameter :: nl = 26
      integer,parameter :: nz = 27








# 29 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/couple/c_coupler/register_private_variables_mod.F90" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/dynamics/eul/comfm2.h" 1

!! (wanhui 2003.07.07)
!! (wanhui 2003.11.04)
!! (b.wang 2004.02.15)


       real*8  du  (nx,ny,nl)  !
       real*8  dv  (nx,ny,nl)  ! tendency at present step
       real*8  dtt (nx,ny,nl)  !

       real*8  du0 (nx,ny,nl)  !
       real*8  dv0 (nx,ny,nl)  !
       real*8  dtt0(nx,ny,nl)  ! tendency at step n-1
       real*8  dps0(nx,ny)     !

       real*8  du1 (nx,ny,nl)  !
       real*8  dv1 (nx,ny,nl)  !
       real*8  dtt1(nx,ny,nl)  ! tendency at step n
       real*8  dps1(nx,ny)     !

       real*8  uu  (nx,ny,nl)  !
       real*8  vv  (nx,ny,nl)  !
       real*8  tt  (nx,ny,nl)  ! variables at present step
       real*8  p   (nx,ny)     !
       real*8  ply (nx,ny,nl)  !

       real*8  up  (nx,ny,nl)  !
       real*8  vp  (nx,ny,nl)  !
       real*8  ttp (nx,ny,nl)  ! variables at step n-1
       real*8  pps (nx,ny)     !
       real*8  dps (nx,ny)     !

       real*8  uk  (nx,ny,nl)  !
       real*8  vk  (nx,ny,nl)  !
       real*8  ttk (nx,ny,nl)  ! variables at step n
       real*8  psk (nx,ny)     !

       real*8  dlt1
       real*8  dlt2

       real*8  tb  (nx,ny,nl)
       real*8  cb  (nx,ny,nl)
       real*8  dcb (nx,ny,nl)

       real*8  hps (nx,ny)
       real*8  c0  (nx,ny)
       real*8  cs0 (nx,ny)
       real*8  cb0 (NX,NY,NL)
       real*8  cbs (NX,NY,NL)
       integer nigw(ny),nzad(ny)

       common/comfm2/ du,dv,dtt, du0,dv0,dtt0,dps0, du1,dv1,dtt1,dps1
       common/comfm2/ uu,vv,tt,p,ply, up,vp,ttp,pps,dps, uk,vk,ttk,psk
       common/comfm2/ dlt1,dlt2, tb,cb,dcb
       common/comfm2/ hps,c0,cb0,nigw
# 30 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/couple/c_coupler/register_private_variables_mod.F90" 2

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
