!
      module coupling_with_CCPL_mod


      USE CMN_SIZE_MOD
      USE GIGC_State_Met_Mod, ONLY : MetState
      USE CCPL_interface_mod


      REAL*8, allocatable   :: CLDF_CCPL(:,:,:)
      REAL*8, allocatable   :: OPTDEP_CCPL(:,:,:)

      CONTAINS


      subroutine register_coupling_fields_to_CCPL(State_Met)
        USE GIGC_State_Met_Mod, ONLY : MetState
        implicit none
        TYPE(MetState), INTENT(INOUT) :: State_Met

        call c_coupler_register_field_info('PS','Pa','sfc press at timestep')
        call c_coupler_register_field_info('CLDF','fraction','Cloud fraction')
        call c_coupler_register_field_info('CMFMC','kg m-2 s-1','Moist convection mass flux')
        call c_coupler_register_field_info('DQRCU','kg m-2 s-1','conv precip prod rate')
        call c_coupler_register_field_info('DQRLSAN','kg m-2 s-1','LS precip prod rate')
        call c_coupler_register_field_info('DQIDTMST','kg kg-1 s-1','ice tendency, mst proc')
        call c_coupler_register_field_info('DQLDTMST','kg kg-1 s-1','H2O tendency, mst proc')
        call c_coupler_register_field_info('DQVDTMST','kg kg-1 s-1','vapor tendency, mst proc')
        call c_coupler_register_field_info('DTRAIN','kg m-2 s-1','detrainment flux')
        call c_coupler_register_field_info('MOISTQ','g kg-1 day-1','tendency in sp. C17')
        call c_coupler_register_field_info('OPTDEP','1','visible optical depth')
        call c_coupler_register_field_info('PV','m2 kg-1 s-1','potential vort')
        call c_coupler_register_field_info('QI','kg kg-1','cloud ice mixing ratio')
        call c_coupler_register_field_info('QL','kg kg-1','cloud water mixing ratio')
        call c_coupler_register_field_info('RH','fraction','relative humidity')
        call c_coupler_register_field_info('SPHU','g kg-1','specific humidity')
        call c_coupler_register_field_info('T','K','temperature')
        call c_coupler_register_field_info('TAUCLI','dimensionless','opt depth of ice clouds')
        call c_coupler_register_field_info('TAUCLW','dimensionless','opt depth of H2O cloud')
        call c_coupler_register_field_info('U','m -s','E/W component of wind')
        call c_coupler_register_field_info('V','m -s','N/S component of wind')
        call c_coupler_register_field_info('ALBD','fraction','visible surface albedo')
        call c_coupler_register_field_info('CLDFRC','fraction','column cloud fraction')
        call c_coupler_register_field_info('CLDTOPS','fraction','column cloud fraction')
        call c_coupler_register_field_info('EFLUX','W m-2','latent heat flux')
        call c_coupler_register_field_info('EVAP','kg m-2 s-1','surface evaporation')
        call c_coupler_register_field_info("FRLAKE",   "fraction", "fraction of lake") 
        call c_coupler_register_field_info('FRLAND','fraction','fraction of land') 
        call c_coupler_register_field_info("FRLANDIC", "fraction", "fraction of land ice") 
        call c_coupler_register_field_info('FROCEAN','fraction','fraction of ocean')
        call c_coupler_register_field_info('GRN','fraction','greenness fraction')
        call c_coupler_register_field_info('GWETROOT','fraction','root zone soil wetness')
        call c_coupler_register_field_info('GWETTOP','fraction','top soil moisture')
        call c_coupler_register_field_info('HFLUX','W m-2','sensible heat flux')
        call c_coupler_register_field_info('LAI','m2 m-2','leaf area index')
        call c_coupler_register_field_info('PARDR','W m-2','direct photsyn active rad')
        call c_coupler_register_field_info('PARDF','W m-2','diffuse photsyn active rad')
        call c_coupler_register_field_info('PBLH','m','PBL height')
        call c_coupler_register_field_info('PRECCON','kg m-2 s-1','conv precip @ ground')
        call c_coupler_register_field_info('PRECTOT','kg m-2 s-1','total precip @ ground')
        call c_coupler_register_field_info('PRECSNO','kg m-2 s-1','snow precip')
        call c_coupler_register_field_info('PS','Pa','sfc press at timestep')
        call c_coupler_register_field_info('RADLWG','W m-2','net LW radiation @ ground')
        call c_coupler_register_field_info('RADSWG','W m-2','solar radiation @ ground')
        call c_coupler_register_field_info('SLP','Pa','sea level pressure')
        call c_coupler_register_field_info('SNODP','m','snow depth')
        call c_coupler_register_field_info('SNOMAS','m','snow mass(total snow storage on the land)')
        call c_coupler_register_field_info('TROPP','Pa','tropopause pressure')
        call c_coupler_register_field_info('TS','K','surface temperature')
        call c_coupler_register_field_info('TSKIN','K','surface skin temperature')
        call c_coupler_register_field_info('U10M','m s-1','E/W wind speed @ 10m height')

        call c_coupler_register_model_data(State_Met%PS1, &
                                               "GIGC_h2D_grid_decomp","PS",.true., grid_name="GIGC_h2D_grid")
        call c_coupler_register_model_data(State_Met%PBLH, &
                                               "GIGC_h2D_grid_decomp","PBLH",.true., grid_name="GIGC_h2D_grid")
        call c_coupler_register_model_data(State_Met%SLP, &
                                               "GIGC_h2D_grid_decomp","SLP",.true., grid_name="GIGC_h2D_grid")
        call c_coupler_register_model_data(State_Met%CLDFRC, &
                                               "GIGC_h2D_grid_decomp","CLDFRC",.true., grid_name="GIGC_h2D_grid")
        call c_coupler_register_model_data(State_Met%EFLUX, &
                                               "GIGC_h2D_grid_decomp","EFLUX",.true., grid_name="GIGC_h2D_grid")
        call c_coupler_register_model_data(State_Met%HFLUX, &
                                               "GIGC_h2D_grid_decomp","HFLUX",.true., grid_name="GIGC_h2D_grid")
        call c_coupler_register_model_data(State_Met%EVAP, &
                                               "GIGC_h2D_grid_decomp","EVAP",.true., grid_name="GIGC_h2D_grid")
        call c_coupler_register_model_data(State_Met%TS, &
                                               "GIGC_h2D_grid_decomp","TS",.true., grid_name="GIGC_h2D_grid")
        call c_coupler_register_model_data(State_Met%TSKIN, &
                                               "GIGC_h2D_grid_decomp","TSKIN",.true., grid_name="GIGC_h2D_grid")
        call c_coupler_register_model_data(State_Met%PARDR, &
                                               "GIGC_h2D_grid_decomp","PARDR",.true., grid_name="GIGC_h2D_grid")
        call c_coupler_register_model_data(State_Met%PARDF, &
                                               "GIGC_h2D_grid_decomp","PARDF",.true., grid_name="GIGC_h2D_grid")
        call c_coupler_register_model_data(State_Met%PRECCON, &
                                               "GIGC_h2D_grid_decomp","PRECCON",.true., grid_name="GIGC_h2D_grid")
        call c_coupler_register_model_data(State_Met%PRECTOT, &
                                               "GIGC_h2D_grid_decomp","PRECTOT",.true., grid_name="GIGC_h2D_grid")
        call c_coupler_register_model_data(State_Met%PRECSNO, &
                                               "GIGC_h2D_grid_decomp","PRECSNO",.true., grid_name="GIGC_h2D_grid")
        call c_coupler_register_model_data(State_Met%RADLWG, &
                                               "GIGC_h2D_grid_decomp","RADLWG",.true., grid_name="GIGC_h2D_grid")
        call c_coupler_register_model_data(State_Met%RADSWG, &
                                               "GIGC_h2D_grid_decomp","RADSWG",.true., grid_name="GIGC_h2D_grid")
        call c_coupler_register_model_data(State_Met%FROCEAN, &
                                               "GIGC_h2D_grid_decomp","FROCEAN",.true., grid_name="GIGC_h2D_grid")
        call c_coupler_register_model_data(State_Met%FRLAKE, &
                                               "GIGC_h2D_grid_decomp","FRLAKE",.true., grid_name="GIGC_h2D_grid")
        call c_coupler_register_model_data(State_Met%FRLAND, &
                                               "GIGC_h2D_grid_decomp","FRLAND",.true., grid_name="GIGC_h2D_grid")
!        call c_coupler_register_sigma_grid_bottom_field(State_Met%PS1,"GIGC_3D_grid")

        call c_coupler_register_model_data(CLDF_CCPL, "GIGC_h2D_grid_decomp","CLDF",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(State_Met%CLDTOPS, &
                                               "GIGC_h2D_grid_decomp","CLDTOPS",.true., grid_name="GIGC_h2D_grid")
!        call c_coupler_register_model_data(State_Met%CMFMC, &
!                                               "GIGC_h2D_grid_decomp","CMFMC",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(State_Met%DQIDTMST, &
                                               "GIGC_h2D_grid_decomp","DQIDTMST",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(State_Met%DQLDTMST, &
                                               "GIGC_h2D_grid_decomp","DQLDTMST",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(State_Met%DQRCU, &
                                               "GIGC_h2D_grid_decomp","DQRCU",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(State_Met%DQRLSAN, &
                                               "GIGC_h2D_grid_decomp","DQRLSAN",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(State_Met%DQVDTMST, &
                                               "GIGC_h2D_grid_decomp","DQVDTMST",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(State_Met%DTRAIN, &
                                               "GIGC_h2D_grid_decomp","DTRAIN",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(State_Met%MOISTQ, &
                                               "GIGC_h2D_grid_decomp","MOISTQ",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(OPTDEP_CCPL, &
                                               "GIGC_h2D_grid_decomp","OPTDEP",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(State_Met%PV, &
                                               "GIGC_h2D_grid_decomp","PV",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(State_Met%RH, &
                                               "GIGC_h2D_grid_decomp","RH",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(State_Met%SPHU, &
                                               "GIGC_h2D_grid_decomp","SPHU",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(State_Met%QL, &
                                               "GIGC_h2D_grid_decomp","QL",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(State_Met%QI, &
                                               "GIGC_h2D_grid_decomp","QI",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(State_Met%T, &
                                               "GIGC_h2D_grid_decomp","T",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(State_Met%TAUCLI, &
                                               "GIGC_h2D_grid_decomp","TAUCLI",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(State_Met%TAUCLW, &
                                               "GIGC_h2D_grid_decomp","TAUCLW",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(State_Met%U, &
                                               "GIGC_h2D_grid_decomp","U",.true., grid_name="GIGC_3D_grid")
        call c_coupler_register_model_data(State_Met%V, &
                                               "GIGC_h2D_grid_decomp","V",.true., grid_name="GIGC_3D_grid")

      end subroutine register_coupling_fields_to_CCPL



      subroutine register_decompositions_to_CCPL
       implicit none
       integer,allocatable :: decomp_cell_indexes(:)
       integer             :: n, i, j

       allocate(decomp_cell_indexes(IIPAR*JJPAR))
       allocate(CLDF_CCPL(IIPAR, JJPAR, LLPAR))
       allocate(OPTDEP_CCPL(IIPAR, JJPAR, LLPAR))

       n = 0
       do j = 1, JJPAR
       do i = 1, IIPAR
          n = n + 1
          decomp_cell_indexes(n)=i+(j-1)*IIPAR
       end do
       end do

       call c_coupler_register_decomposition("GIGC_h2D_grid_decomp", "GIGC_h2D_grid", n, decomp_cell_indexes)

       deallocate(decomp_cell_indexes)
            
      end subroutine register_decompositions_to_CCPL


      subroutine transform_CCPL_arrays(State_Met)
      USE GIGC_State_Met_Mod, ONLY : MetState
      implicit none
      TYPE(MetState), INTENT(INOUT) :: State_Met
      integer       :: i, j, k

      do k = 1, LLPAR
      do j = 1, JJPAR
      do i = 1, IIPAR
         State_Met%CLDF(k,i,j) = CLDF_CCPL(i,j,k)
         State_Met%OPTDEP(k,i,j) = OPTDEP_CCPL(i,j,k)
      enddo
      enddo
      enddo

      end subroutine transform_CCPL_arrays


      end module coupling_with_CCPL_mod
