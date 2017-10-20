!***************************************************************
!  This is a source file of GAMIL, which registers all parallel 
!  decompositions into C-Coupler library. This file was initially 
!  finished by Dr. Li Liu. If you have any problem, please 
!  contact Dr. Li Liu via liuli-cess@tsinghua.edu.cn
!***************************************************************


module register_decompositions_mod

    public register_decompositions

contains

    subroutine register_decompositions
       use pmgrid
       use phys_grid
       use rgrid,          only: nlon                                                  ! reduced grid
       use CCPL_interface_mod
       implicit none
       integer,allocatable :: decomp_cell_indexes(:)
       integer,allocatable :: dyn_cell_global_index(:)
       integer             :: n, i, j, startpoint, bufsize
       integer :: lchnk         ! indices
       integer :: ncol                  ! number of columns in current chunk
       integer :: lats(pcols)           ! array of latitude indices
       integer :: lons(pcols)           ! array of longitude indices
       integer :: begj, endj, NX

       NX=130

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
!       call c_coupler_register_decomposition("gamil_gamil_grid_decomp", "gamil_grid", n, decomp_cell_indexes)
       deallocate(decomp_cell_indexes)

       allocate(decomp_cell_indexes(pcols*(endchunk-begchunk+1)))
       decomp_cell_indexes=0
       do lchnk = begchunk,endchunk
          ncol = get_ncols_p(lchnk)
          call get_lon_all_p(lchnk, ncol, lons)
          call get_lat_all_p(lchnk, ncol, lats)
          do i=1,ncol
              decomp_cell_indexes((lchnk-begchunk)*pcols+i)=(lats(i)-1)*(128)+lons(i)
          end do
       end do
!       call c_coupler_register_decomposition("gamil_2D_decomp_phys", "gamil_grid", &
                                  !pcols*(endchunk-begchunk+1), decomp_cell_indexes)
       deallocate(decomp_cell_indexes)

       bufsize=(endlatexdyn-beglatexdyn+1)*NX
       allocate(dyn_cell_global_index(bufsize))
       dyn_cell_global_index=-1
       begj = beglatexdyn + numbnd
       endj = endlatexdyn - numbnd
       do j=begj,endj
          do i=2,NX-1
             dyn_cell_global_index((j-begj)*NX+i) = (j-1)*(NX-2)+i-1
          enddo
       enddo

!       call c_coupler_register_decomposition("gamil_2D_decomp_dyn", "gamil_grid", &
!                                  bufsize, dyn_cell_global_index)

       deallocate(dyn_cell_global_index)

    end subroutine register_decompositions

end module register_decompositions_mod

