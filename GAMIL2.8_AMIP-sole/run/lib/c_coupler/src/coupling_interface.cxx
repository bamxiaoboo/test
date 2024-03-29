/***************************************************************
  *  Copyright (c) 2017, Tsinghua University.
  *  This is a source file of C-Coupler.
  *  This file was initially finished by Dr. Li Liu. 
  *  If you have any problem, 
  *  please contact Dr. Li Liu via liuli-cess@tsinghua.edu.cn
  ***************************************************************/


#include <mpi.h>
#include "global_data.h"
#include "cor_global_data.h"
#include "remap_mgt.h"
#include "quick_sort.h"
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include "coupling_interface.h"


int coupling_process_control_counter = 0;


void check_for_component_registered(int comp_id, int API_ID, const char *annotation, bool enable_minus_1)
{
	char API_label[NAME_STR_SIZE];
	

	get_API_hint(-1, API_ID, API_label);
	check_for_ccpl_managers_allocated(API_ID, annotation);

	if (comp_id == -1)
		EXECUTION_REPORT(REPORT_ERROR, comp_id, enable_minus_1, "The component ID (-1) is wrong when calling the C-Coupler API \"%s\". Please check the model code with the annotation \"%s\"", API_label, annotation);
	
	if (comp_id != -1)
		EXECUTION_REPORT(REPORT_ERROR, -1, comp_comm_group_mgt_mgr->is_legal_local_comp_id(comp_id) && comp_comm_group_mgt_mgr->get_current_proc_id_in_comp(comp_id, "in check_for_component_registered") >= 0, "The component ID is wrong when calling the C-Coupler API \"%s\". Please check the model code with the annotation \"%s\"", API_label, annotation);
}


extern "C" void finalize_ccpl_(int *to_finalize_MPI)
{
	if (comp_comm_group_mgt_mgr->get_current_proc_global_id() == 0)
		EXECUTION_REPORT(REPORT_PROGRESS, -1, true, "Start to finalize C-Coupler");
	
	inout_interface_mgr->free_all_MPI_wins();

	delete annotation_mgr;
	delete decomps_info_mgr;
	delete decomp_grids_mgr;
	delete components_time_mgrs;
	delete timer_mgr;
	delete inout_interface_mgr;
	delete routing_info_mgr;
	delete IO_fields_mgr;
	delete components_IO_output_procedures_mgr;
	delete fields_gather_scatter_mgr;
	delete remapping_configuration_mgr;
	delete runtime_remapping_weights_mgr;
	delete fields_info;
	delete original_grid_mgr;
	delete comp_comm_group_mgt_mgr;
	delete memory_manager;
	delete coupling_generator;
	comp_comm_group_mgt_mgr = NULL;

	if (*to_finalize_MPI == 0)
		return;
	int flag;
	MPI_Finalized(&flag);
	if (!flag)
		MPI_Finalize();
}


extern "C" void get_ccpl_double_current_calendar_time_(int *comp_id, double *cal_time, int *shift_seconds, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_GET_CURRENT_CAL_TIME, annotation, false);
    *cal_time = components_time_mgrs->get_time_mgr(*comp_id)->get_double_current_calendar_time(*shift_seconds, annotation);
}


extern "C" void get_ccpl_float_current_calendar_time_(int *comp_id, float *cal_time, int *shift_seconds, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_GET_CURRENT_CAL_TIME, annotation, false);
    *cal_time = components_time_mgrs->get_time_mgr(*comp_id)->get_float_current_calendar_time(*shift_seconds, annotation);
}


extern "C" void get_ccpl_current_date_(int *comp_id, int *date, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_GET_CURRENT_DATE, annotation, false);
    *date = components_time_mgrs->get_time_mgr(*comp_id)->get_current_date();
}


extern "C" void get_ccpl_current_second_(int *comp_id, int *second, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_GET_CURRENT_SECOND, annotation, false);
    *second = components_time_mgrs->get_time_mgr(*comp_id)->get_current_second();
}


extern "C" void is_comp_first_step_(int *comp_id, int *result, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_IS_FIRST_STEP, annotation, false);
	*result = components_time_mgrs->get_time_mgr(*comp_id)->get_current_num_time_step() == 0? 1 : 0;
}


extern "C" void is_comp_first_restart_step_(int *comp_id, int *result, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_IS_FIRST_RESTART_STEP, annotation, false);
	*result = components_time_mgrs->get_time_mgr(*comp_id)->is_first_restart_step()? 1 : 0;
}


extern "C" void get_ccpl_current_number_of_step_(int *comp_id, int *nstep, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_GET_NUM_CURRENT_STEP, annotation, false);
	*nstep = components_time_mgrs->get_time_mgr(*comp_id)->get_current_num_time_step();
}


extern "C" void get_ccpl_num_total_step_(int *comp_id, int *nstep, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_GET_NUM_TOTAL_STEPS, annotation, false);
	*nstep = (int) components_time_mgrs->get_time_mgr(*comp_id)->get_num_total_step();
}


extern "C" void get_ccpl_time_step_(int *comp_id, int *time_step, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_GET_TIME_STEP, annotation, false);
    *time_step = components_time_mgrs->get_time_mgr(*comp_id)->get_time_step_in_second();
}


extern "C" void get_ccpl_start_time_(int *comp_id, int *year, int *month, int *day, int *seconds, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_GET_START_TIME, annotation, false);

	*year = components_time_mgrs->get_time_mgr(*comp_id)->get_start_full_time() / 1000000000;
	*month = (components_time_mgrs->get_time_mgr(*comp_id)->get_start_full_time() / 10000000)%100;
	*day = (components_time_mgrs->get_time_mgr(*comp_id)->get_start_full_time() / 100000)%100;
	*seconds = components_time_mgrs->get_time_mgr(*comp_id)->get_start_full_time() % 100000;
}


extern "C" void get_ccpl_stop_time_(int *comp_id, int *year, int *month, int *day, int *second, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_GET_STOP_TIME, annotation, false);

	*year = components_time_mgrs->get_time_mgr(*comp_id)->get_stop_year();
	*month = components_time_mgrs->get_time_mgr(*comp_id)->get_stop_month();
	*day = components_time_mgrs->get_time_mgr(*comp_id)->get_stop_day();
	*second = components_time_mgrs->get_time_mgr(*comp_id)->get_stop_second();
}


extern "C" void get_ccpl_previous_time_(int *comp_id, int *year, int *month, int *day, int *seconds, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_GET_PREVIOUS_TIME, annotation, false);

	*year = components_time_mgrs->get_time_mgr(*comp_id)->get_previous_full_time() / 1000000000;
	*month = (components_time_mgrs->get_time_mgr(*comp_id)->get_previous_full_time() / 10000000)%100;
	*day = (components_time_mgrs->get_time_mgr(*comp_id)->get_previous_full_time() / 100000)%100;
	*seconds = components_time_mgrs->get_time_mgr(*comp_id)->get_previous_full_time() % 100000;
}


extern "C" void get_ccpl_current_time_(int *comp_id, int *year, int *month, int *day, int *second, int *shift_second, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_GET_CURRENT_TIME, annotation, false);
	components_time_mgrs->get_time_mgr(*comp_id)->get_current_time(*year, *month, *day, *second, *shift_second, annotation);
}


extern "C" void get_ccpl_current_num_days_in_year_(int *comp_id, int *days, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_GET_CURRENT_NUM_DAYS_IN_YEAR, annotation, false);
	*days = components_time_mgrs->get_time_mgr(*comp_id)->get_current_num_days_in_year();
}


extern "C" void get_ccpl_current_year_(int *comp_id, int *year, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_GET_CURRENT_YEAR, annotation, false);
	*year = components_time_mgrs->get_time_mgr(*comp_id)->get_current_year();
}


extern "C" void get_ccpl_num_elapsed_days_from_start_date_(int *comp_id, int *days, int *seconds, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_GET_ELAPSED_DAYS_FROM_START, annotation, false);
	components_time_mgrs->get_time_mgr(*comp_id)->get_elapsed_days_from_start_date(days, seconds);
}


extern "C" void get_ccpl_num_elapsed_days_from_reference_date_(int *comp_id, int *days, int *seconds, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_GET_ELAPSED_DAYS_FROM_REF, annotation, false);
	components_time_mgrs->get_time_mgr(*comp_id)->get_elapsed_days_from_reference_date(days, seconds);
}


extern "C" void coupling_abort_(const char *error_string)
{
        EXECUTION_REPORT(REPORT_ERROR,-1, false, error_string);
}


extern "C" void initialize_ccpl_mgrs_()
{
	execution_phase_number = 1;
	annotation_mgr = new Annotation_mgt();
	decomps_info_mgr = new Decomp_info_mgt();
	decomp_grids_mgr = new Decomp_grid_mgt();
	memory_manager = new Memory_mgt();
	components_time_mgrs = new Components_time_mgt();
	timer_mgr = new Timer_mgt();
	execution_phase_number = 2;
	inout_interface_mgr = new Inout_interface_mgt();
	IO_fields_mgr = new IO_field_mgt();
	components_IO_output_procedures_mgr = new Components_IO_output_procedures_mgt();
	fields_gather_scatter_mgr = new Fields_gather_scatter_mgt();
	remapping_configuration_mgr = new Remapping_configuration_mgt();
	routing_info_mgr = new Routing_info_mgt();
	runtime_remapping_weights_mgr = new Runtime_remapping_weights_mgt();
	all_H2D_remapping_wgt_files_info = new H2D_remapping_wgt_file_container();
	coupling_generator = new Coupling_generator();
}


extern "C" void check_fortran_api_int_type_(int *fortran_int_size)
{
	EXECUTION_REPORT(REPORT_ERROR, -1, *fortran_int_size == 4, "Error happens when using C-Coupler for model coupling: the size of an integer value in C-Coupler FORTRAN APIs is not 4 types. Please verify the compiler flag for C-Coupler and then recompile C-Coupler, to force the usage of 4-byte integer.");
}



extern "C" void register_root_component_(MPI_Comm *comm, const char *comp_name, const char *local_comp_type, const char *annotation, int *comp_id, 
										const char *executable_name)
{
	int flag;
	MPI_Comm local_comm = -1;
	int root_comp_id;
	int current_proc_global_id;
	char file_name[NAME_STR_SIZE];


	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "start to register the root component model");

	check_and_verify_name_format_of_string_for_API(-1, comp_name, API_ID_COMP_MGT_REG_COMP, "the root component", annotation);

	if (comp_comm_group_mgt_mgr != NULL) 
		EXECUTION_REPORT(REPORT_ERROR, -1, comp_comm_group_mgt_mgr == NULL, "Error happens when registering the root component (\"%s\") at the model code with the annotation \"%s\": the root compnent has been registered before at the model code with the annotation \"%s\"", comp_name, annotation, comp_comm_group_mgt_mgr->get_annotation_start());
	MPI_Initialized(&flag);
	if (flag == 0) {
		EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Initialize MPI when registerring the root component \"%s\"", comp_name);
		MPI_Init(NULL, NULL);
	}

	synchronize_comp_processes_for_API(-1, API_ID_COMP_MGT_REG_COMP, MPI_COMM_WORLD, "registering root component", annotation);

	comp_comm_group_mgt_mgr = new Comp_comm_group_mgt_mgr(executable_name);

	if (*comm != -1) {
		EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Before MPI_barrier at root component \"%s\" for synchronizing the processes of the component (the corresponding model code annotation is \"%s\").", comp_name, annotation);
		EXECUTION_REPORT(REPORT_ERROR,-1, MPI_Barrier(*comm) == MPI_SUCCESS);
		EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "After MPI_barrier at root component \"%s\" for synchronizing the processes of the component (the corresponding model code annotation is \"%s\").", comp_name, annotation);
		
	}

	root_comp_id = comp_comm_group_mgt_mgr->register_component(comp_name, local_comp_type, local_comm, -1, annotation);

	if (*comm != -1) {
		int input_comm_size, new_comm_size;
		int *input_comm_process_ids, *new_comm_process_ids, *temp_array;
		int current_proc_global_id, current_proc_local_id;
		MPI_Comm new_comm;
		EXECUTION_REPORT(REPORT_ERROR,-1, MPI_Comm_size(*comm, &input_comm_size) == MPI_SUCCESS);
		new_comm = comp_comm_group_mgt_mgr->get_comm_group_of_local_comp(root_comp_id, "C-Coupler code in register_root_component for getting component management node");
		EXECUTION_REPORT(REPORT_ERROR,-1, MPI_Comm_size(new_comm, &new_comm_size) == MPI_SUCCESS);
		EXECUTION_REPORT(REPORT_ERROR,-1, input_comm_size == new_comm_size);  // add debug information
		EXECUTION_REPORT(REPORT_ERROR,-1, MPI_Comm_rank(MPI_COMM_WORLD, &current_proc_global_id) == MPI_SUCCESS);
		input_comm_process_ids = new int [input_comm_size];
		new_comm_process_ids = new int [new_comm_size];
		temp_array = new int [new_comm_size];
		EXECUTION_REPORT(REPORT_ERROR,-1, MPI_Allgather(&current_proc_global_id, 1, MPI_INT, input_comm_process_ids, 1, MPI_INT, *comm) == MPI_SUCCESS);
		EXECUTION_REPORT(REPORT_ERROR,-1, MPI_Allgather(&current_proc_global_id, 1, MPI_INT, new_comm_process_ids, 1, MPI_INT, new_comm) == MPI_SUCCESS);
		do_quick_sort(input_comm_process_ids, temp_array, 0, input_comm_size-1);
		do_quick_sort(new_comm_process_ids, temp_array, 0, new_comm_size-1);
		for (int i = 0; i < input_comm_size; i ++)
			EXECUTION_REPORT(REPORT_ERROR,-1, input_comm_process_ids[i] == new_comm_process_ids[i], 
			                 "The communicator of root component \"%s\" does not match the communicator generated (processes of the two communicators are not the same). Please check the model code with the annotation \"%s\"",
			                 comp_name, annotation);
		delete [] input_comm_process_ids;
		delete [] new_comm_process_ids;
		delete [] temp_array;
	}
	else *comm = local_comm;

	*comp_id = root_comp_id;

	sprintf(file_name, "%s/all/env_run.xml", comp_comm_group_mgt_mgr->get_config_root_dir());
	components_time_mgrs->define_root_comp_time_mgr(root_comp_id, file_name);
	import_report_setting();
	fields_info = new Field_info_mgt();
	original_grid_mgr = new Original_grid_mgt();
	remapping_configuration_mgr->add_remapping_configuration(comp_comm_group_mgt_mgr->get_global_node_root()->get_comp_id());
	if (comp_comm_group_mgt_mgr->get_global_node_of_local_comp(root_comp_id,"")->is_real_component_model())
		remapping_configuration_mgr->add_remapping_configuration(root_comp_id);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish registering the root component model");
}


extern "C" void register_component_(int *parent_comp_id, const char *comp_name, const char *local_comp_type, MPI_Comm *comm, const char *annotation, int *comp_id)
{
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "start to register component model \%s\"", comp_name);

	check_and_verify_name_format_of_string_for_API(-1, comp_name, API_ID_COMP_MGT_REG_COMP, "the new component", annotation);
	check_for_coupling_registration_stage(*parent_comp_id, API_ID_COMP_MGT_REG_COMP, false, annotation);

	if (*comm !=-1) {
		synchronize_comp_processes_for_API(*parent_comp_id, API_ID_COMP_MGT_REG_COMP, *comm, "registering a component based on the parent component", annotation);
		check_API_parameter_string(*parent_comp_id, API_ID_COMP_MGT_REG_COMP, *comm, "registering a component based on an available communicator", comp_name, "comp_name", annotation);
	}
	else synchronize_comp_processes_for_API(*parent_comp_id, API_ID_COMP_MGT_REG_COMP, comp_comm_group_mgt_mgr->get_comm_group_of_local_comp(*parent_comp_id, "C-Coupler code for get comm group in register_component interface"), "registering component based on the parent component", annotation);

	*comp_id = comp_comm_group_mgt_mgr->register_component(comp_name, local_comp_type, *comm, *parent_comp_id, annotation);
	if (comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id,"")->is_real_component_model())
		remapping_configuration_mgr->add_remapping_configuration(*comp_id);
	components_time_mgrs->clone_parent_comp_time_mgr(*comp_id, *parent_comp_id, annotation);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish registering component model \%s\"", comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id,"")->get_full_name());
}


extern "C" void get_id_of_component_(const char *comp_name, const char *annotation, int *comp_id)
{
	check_and_verify_name_format_of_string_for_API(-1, comp_name, API_ID_COMP_MGT_GET_COMP_ID, "the component", annotation);
	check_for_component_registered(-1, API_ID_COMP_MGT_GET_COMP_ID, annotation, true);

	Comp_comm_group_mgt_node *node = comp_comm_group_mgt_mgr->search_comp_with_comp_name(comp_name);

	if (node == NULL) {
		EXECUTION_REPORT(REPORT_ERROR, -1, false, "Error happens when calling API \"CCPL_get_component_id\" to get the ID of component \"%s\": no component with the name of \"%s\" has been registerred. Please check the model code at the annotation \"%s\"", comp_name, comp_name, annotation);
		*comp_id = -1;
	}
	else *comp_id = node->get_local_node_id();
	
}


extern "C" void is_current_process_in_component_(const char *comp_full_name, int *is_in_comp, const char *annotation)
{
	check_for_component_registered(-1, API_ID_COMP_MGT_IS_CURRENT_PROC_IN_COMP, annotation, true);
	Comp_comm_group_mgt_node *comp_node = comp_comm_group_mgt_mgr->search_global_node(comp_full_name);
	*is_in_comp = comp_node != NULL && comp_node->get_current_proc_local_id() != -1? 1 : 0;
}


extern "C" void get_current_proc_id_in_comp_(int *comp_id, int *proc_id, const char * annotation)
{
	check_for_component_registered(*comp_id, API_ID_COMP_MGT_GET_CURRENT_PROC_ID_IN_COMP, annotation, false);
	*proc_id = comp_comm_group_mgt_mgr->get_current_proc_id_in_comp(*comp_id, annotation);
}


extern "C" void get_num_proc_in_comp_(int *comp_id, int *num_proc, const char * annotation)
{
	check_for_component_registered(*comp_id, API_ID_COMP_MGT_GET_NUM_PROC_IN_COMP, annotation, false);
	*num_proc = comp_comm_group_mgt_mgr->get_num_proc_in_comp(*comp_id, annotation);
}


extern "C" void get_comp_proc_global_id_(int *comp_id, int *local_proc_id, int *global_proc_id, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_COMP_MGT_GET_COMP_PROC_GLOBAL_ID, annotation, false);
	int num_proc = comp_comm_group_mgt_mgr->get_num_proc_in_comp(*comp_id, annotation);
	EXECUTION_REPORT(REPORT_ERROR, *comp_id, *local_proc_id >= 0 && *local_proc_id < num_proc, "Error happens when calling the API \"CCPL_get_component_process_global_id\": the parameter \"local_proc_id\" is wrong (its value is %d, not between 0 and %d). Please verify the model code corresponding to the annotation \"%s\"", *local_proc_id, num_proc-1, annotation);
	*global_proc_id = comp_comm_group_mgt_mgr->search_global_node(*comp_id)->get_local_proc_global_id(*local_proc_id);
}


extern "C" void end_registration_(int *comp_id, const char * annotation)
{
	check_for_component_registered(*comp_id, API_ID_COMP_MGT_END_COMP_REG, annotation, false);
	
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "start to end the coupling registration for the component model \"%s\"", comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id,"")->get_full_name());
	
	synchronize_comp_processes_for_API(*comp_id, API_ID_COMP_MGT_END_COMP_REG, comp_comm_group_mgt_mgr->get_comm_group_of_local_comp(*comp_id, "C-Coupler code in register_component for getting component management node"), "first synchorization for ending the registration of a component", annotation);	

	comp_comm_group_mgt_mgr->merge_comp_comm_info(*comp_id, annotation);
	inout_interface_mgr->merge_unconnected_inout_interface_fields_info(*comp_id);
	if (((*comp_id) & TYPE_ID_SUFFIX_MASK) == 1) {
		coupling_generator->generate_coupling_procedures();
		coupling_generator->generate_IO_procedures();
		delete all_H2D_remapping_wgt_files_info;
	}
	synchronize_comp_processes_for_API(*comp_id, API_ID_COMP_MGT_END_COMP_REG, comp_comm_group_mgt_mgr->get_comm_group_of_local_comp(*comp_id, "C-Coupler code in register_component for getting component management node"), "second synchorization for ending the registration of a component", annotation);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish ending the coupling registration for the component model \"%s\"", comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id,"")->get_full_name());
	EXECUTION_REPORT(REPORT_PROGRESS, *comp_id, true, "The coupling registration stage of the component model \"%s\" is successfully ended at the model code with the annotation \"%s\"", comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id,"")->get_full_name(), annotation);
}


extern "C" void register_v1d_grid_with_data_(int *comp_id, int *grid_id, const char *grid_name, int *grid_type, const char *coord_unit, int *dim_size2,  
	                                         int *dim_size3, const char *data_type, void *value1, void *value2, void *value3, const char *annotation)
{
	double temp_value1, *temp_value2, *temp_value3;
	int API_id;
	char API_label[NAME_STR_SIZE];


	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "start to register V1D grid %s", grid_name);
		
	switch (*grid_type) {
		case 1:
			API_id = API_ID_GRID_MGT_REG_V1D_Z_GRID_VIA_MODEL;
			break;	
		case 2:			
			API_id = API_ID_GRID_MGT_REG_V1D_SIGMA_GRID_VIA_MODEL;
			break;
		case 3:			
			API_id = API_ID_GRID_MGT_REG_V1D_HYBRID_GRID_VIA_MODEL;
			break;			
		default:
			EXECUTION_REPORT(REPORT_ERROR, -1, "Software error in register_V1D_grid_with_data: wrong caller_label");
			break;
	}

	common_checking_for_grid_registration(*comp_id, grid_name, coord_unit, API_id, annotation);

	get_API_hint(*comp_id, API_id, API_label);
	EXECUTION_REPORT(REPORT_ERROR, *comp_id, *dim_size2 != 0 && *dim_size3 != 0, "Error happens when calling the C-Coupler API \"%s\" to register a V1D grid \"%s\": some parameters of array have not be allocated. Please verify the model code with the annotation \"%s\" (please make sure all the arrays of grid data have been allocated)", API_label, grid_name, annotation);
	EXECUTION_REPORT(REPORT_ERROR, *comp_id, *dim_size2 > 1 && *dim_size3 == *dim_size2, "Error happens when calling the C-Coupler API \"%s\" to register a V1D grid \"%s\": the implicit grid size that is determined by the parameter arrays is wrong: grid size is smaller than 2 or the sizes of two paramenter arrays are different. Please verify the model code with the annotation \"%s\".", API_label, grid_name, annotation);
	EXECUTION_REPORT(REPORT_ERROR, -1, words_are_the_same(data_type, DATA_TYPE_FLOAT) || words_are_the_same(data_type, DATA_TYPE_DOUBLE), "Software error in register_V1D_grid_with_data: wrong data type");
	temp_value2 = new double [*dim_size2];
	temp_value3 = new double [*dim_size3];
	if (words_are_the_same(data_type, DATA_TYPE_FLOAT)) {
		transform_datatype_of_arrays((float*)value1, &temp_value1, 1);
		transform_datatype_of_arrays((float*)value2, temp_value2, *dim_size2);
		transform_datatype_of_arrays((float*)value3, temp_value3, *dim_size3);
	}
	else {
		transform_datatype_of_arrays((double*)value1, &temp_value1, 1);
		transform_datatype_of_arrays((double*)value2, temp_value2, *dim_size2);
		transform_datatype_of_arrays((double*)value3, temp_value3, *dim_size3);
	}

	EXECUTION_REPORT(REPORT_ERROR, *comp_id, is_array_in_sorting_order(temp_value2,*dim_size2) != 0 && is_array_in_sorting_order(temp_value3,*dim_size2) != 0, "Error happens when calling the C-Coupler API \"%s\" to register a V1D grid \"%s\": some arrays of parameters are not in a descending/ascending order. Please check the model code with the annotation \"%s\".", API_label, grid_name, annotation);
	EXECUTION_REPORT(REPORT_ERROR, *comp_id, is_array_in_sorting_order(temp_value2,*dim_size2) == is_array_in_sorting_order(temp_value3,*dim_size2), "Error happens when calling the C-Coupler API \"%s\" to register a V1D grid \"%s\": the two arrays of parameters are not in the same sorting order. Please check the model code with the annotation \"%s\".", API_label, grid_name, annotation);	
	*grid_id = original_grid_mgr->register_V1D_grid_via_data(API_id, *comp_id, grid_name, *grid_type, coord_unit, *dim_size2, temp_value1, temp_value2, temp_value3, annotation);

	delete [] temp_value2;
	delete [] temp_value3;

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finsh registering V1D grid %s", grid_name);
}


extern "C" void set_3d_grid_surface_field_(int *grid_id, int *field_id, int *static_or_dynamic_or_external, const char *annotation)
{
	char API_label[NAME_STR_SIZE];
	int comp_id, API_id;


	if (*static_or_dynamic_or_external == BOTTOM_FIELD_VARIATION_STATIC)
		API_id = API_ID_GRID_MGT_SET_3D_GRID_STATIC_BOT_FLD;
	else if (*static_or_dynamic_or_external == BOTTOM_FIELD_VARIATION_DYNAMIC) 
		API_id = API_ID_GRID_MGT_SET_3D_GRID_DYN_BOT_FLD;
	else if (*static_or_dynamic_or_external == BOTTOM_FIELD_VARIATION_EXTERNAL) 
		API_id = API_ID_GRID_MGT_SET_3D_GRID_EXTERNAL_BOT_FLD;
	else EXECUTION_REPORT(REPORT_ERROR, -1, false, "software error in set_3d_grid_surface_field_: wrong value of static_or_dynamic_or_external");
	get_API_hint(-1, API_id, API_label);	
	check_for_component_registered(-1, API_id, annotation, true);
	EXECUTION_REPORT(REPORT_ERROR, -1, original_grid_mgr->is_grid_id_legal(*grid_id), "Error happens when calling API \"%s\" to set the surface field of a 3-D grid: the parameter of \"grid_id\" is wrong. Please verify the model code with the annotation \"%s.", API_label, annotation);
	
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to set surface field for the 3D grid %s", original_grid_mgr->get_name_of_grid(*grid_id));
	
	comp_id = original_grid_mgr->get_comp_id_of_grid(*grid_id);
	if (*static_or_dynamic_or_external != BOTTOM_FIELD_VARIATION_EXTERNAL) {
		EXECUTION_REPORT(REPORT_ERROR, comp_id, memory_manager->check_is_legal_field_instance_id(*field_id), "Error happens when calling API \"%s\" to set the surface field of a 3-D grid: the parameter of \"field_id\" is wrong. Please verify the model code with the annotation \"%s.", API_label, annotation);
		EXECUTION_REPORT(REPORT_ERROR, comp_id, comp_id == memory_manager->get_field_instance(*field_id)->get_comp_id(), "Error happens when calling API \"%s\" to set the surface field of a 3-D grid: the components corresponding to the parameters of \"grid_id\" and \"field_id\" are different. Please verify the model code with the annotation \"%s.", API_label, annotation);
	}	
	check_for_coupling_registration_stage(comp_id, API_id, true, annotation);
	original_grid_mgr->set_3d_grid_bottom_field(comp_id, *grid_id, *field_id, *static_or_dynamic_or_external, API_id, API_label, annotation);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish to setting surface field for the 3D grid %s", original_grid_mgr->get_name_of_grid(*grid_id));
}


extern "C" void register_md_grid_via_multi_grids_(int *comp_id, int *grid_id, const char *grid_name, int *sub_grid1_id, int *sub_grid2_id, int *sub_grid3_id, int *size_mask, int *mask, const char *annotation)
{
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to register an MD grid %s", grid_name);

	common_checking_for_grid_registration(*comp_id, grid_name, NULL, API_ID_GRID_MGT_REG_MD_GRID_VIA_MULTI_GRIDS, annotation);
	*grid_id = original_grid_mgr->register_md_grid_via_multi_grids(*comp_id, grid_name, *sub_grid1_id, *sub_grid2_id, *sub_grid3_id, *size_mask, mask, annotation);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish registering an MD grid %s", grid_name);
}


extern "C" void register_h2d_grid_with_global_data_(int *comp_id, int *grid_id, const char *grid_name, const char *edge_type, const char *coord_unit, const char *cyclic_or_acyclic, const char *data_type, int *dim_size1, int *dim_size2, int *size_center_lon, int *size_center_lat, 
	                                        int *size_mask, int *size_area, int *size_vertex_lon, int *size_vertex_lat, char *min_lon, char *max_lon, char *min_lat, char *max_lat, char *center_lon, char *center_lat, int *mask, char *area, char *vertex_lon, char *vertex_lat, const char *annotation)
{
	common_checking_for_grid_registration(*comp_id, grid_name, coord_unit, API_ID_GRID_MGT_REG_H2D_GRID_VIA_GLOBAL_DATA, annotation);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to register an H2D grid %s", grid_name);

	*grid_id = original_grid_mgr->register_H2D_grid_via_global_data(*comp_id, grid_name, edge_type, coord_unit, cyclic_or_acyclic, data_type, *dim_size1, *dim_size2, *size_center_lon, *size_center_lat, 
												                    *size_mask, *size_area, *size_vertex_lon, *size_vertex_lat, min_lon, max_lon, min_lat, max_lat, center_lon, center_lat, mask, area, vertex_lon, vertex_lat, annotation,
												                    API_ID_GRID_MGT_REG_H2D_GRID_VIA_GLOBAL_DATA);
	char nc_file_name[NAME_STR_SIZE];
	sprintf(nc_file_name, "%s/%s@%s.nc", comp_comm_group_mgt_mgr->get_internal_H2D_grids_dir(), grid_name, comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id, annotation)->get_full_name());
	char temp_grid_name[NAME_STR_SIZE];
	sprintf(temp_grid_name, "%s_temp", grid_name);
	original_grid_mgr->register_H2D_grid_via_file(*comp_id, temp_grid_name, nc_file_name, annotation);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish registering an H2D grid %s", grid_name);
}


extern "C" void register_h2d_grid_with_local_data_(int *comp_id, int *grid_id, const char *grid_name, const char *edge_type, const char *coord_unit, const char *cyclic_or_acyclic, const char *data_type, int *grid_size, int *num_local_cells, int *size_local_cells_global_index, int *size_center_lon, int *size_center_lat, 
	                                        int *size_mask, int *size_area, int *size_vertex_lon, int *size_vertex_lat, int *local_cells_global_index, char *min_lon, char *max_lon, char *min_lat, char *max_lat, char *center_lon, char *center_lat, int *mask, char *area, char *vertex_lon, char *vertex_lat, const char *decomp_name, int *decomp_id, const char *annotation)
{
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to register an H2D grid %s", grid_name);

	common_checking_for_grid_registration(*comp_id, grid_name, coord_unit, API_ID_GRID_MGT_REG_H2D_GRID_VIA_LOCAL_DATA, annotation);
	*grid_id = original_grid_mgr->register_H2D_grid_via_local_data(*comp_id, grid_name, edge_type, coord_unit, cyclic_or_acyclic, data_type, *grid_size, *num_local_cells, *size_local_cells_global_index, *size_center_lon, *size_center_lat, *size_mask, *size_area, 
	                                                               *size_vertex_lon, *size_vertex_lat, local_cells_global_index, min_lon, max_lon, min_lat, max_lat, center_lon, center_lat, mask, area, vertex_lon, vertex_lat, decomp_name, decomp_id, annotation, API_ID_GRID_MGT_REG_H2D_GRID_VIA_LOCAL_DATA);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish registering an H2D grid %s", grid_name);
}


extern "C" void register_h2d_grid_with_file_(int *comp_id, int *grid_id, const char *grid_name, const char *data_file_name, const char *annotation)
{
	char full_data_file_name[NAME_STR_SIZE];


	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to register an H2D grid %s", grid_name);

	common_checking_for_grid_registration(*comp_id, grid_name, NULL, API_ID_GRID_MGT_REG_H2D_GRID_VIA_FILE, annotation);
	sprintf(full_data_file_name, "%s/grids_weights/%s", comp_comm_group_mgt_mgr->get_config_exe_dir(), data_file_name);
	*grid_id = original_grid_mgr->register_H2D_grid_via_file(*comp_id, grid_name, full_data_file_name, annotation);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish registering an H2D grid %s", grid_name);
}


extern "C" void register_h2d_grid_from_another_component_(int *comp_id, int *grid_id, const char *grid_name, const char *annotation)
{
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to register an H2D grid %s", grid_name);

	common_checking_for_grid_registration(*comp_id, grid_name, NULL, API_ID_GRID_MGT_REG_H2D_GRID_VIA_COMP, annotation);
	check_and_verify_name_format_of_string_for_API(*comp_id, grid_name, API_ID_GRID_MGT_REG_H2D_GRID_VIA_COMP, "the C-Coupler grid", annotation);
	*grid_id = original_grid_mgr->register_H2D_grid_via_comp(*comp_id, grid_name, annotation);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish registering an H2D grid %s", grid_name);
}


extern "C" void register_cor_defined_grid_(int *comp_id, const char *CCPL_grid_name, const char *CoR_grid_name, const char *annotation, int *grid_id)
{
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to register a CoR grid %s", CCPL_grid_name);

	common_checking_for_grid_registration(*comp_id, CCPL_grid_name, NULL, API_ID_GRID_MGT_REG_GRID_VIA_COR, annotation);
	check_and_verify_name_format_of_string_for_API(*comp_id, CoR_grid_name, API_ID_GRID_MGT_REG_GRID_VIA_COR, "the CoR grid", annotation);
	check_API_parameter_string(*comp_id, API_ID_GRID_MGT_REG_GRID_VIA_COR, comp_comm_group_mgt_mgr->get_comm_group_of_local_comp(*comp_id, "C-Coupler code in register_cor_defined_grid for getting component management node"), "registering a grid", CCPL_grid_name, "CCPL_grid_name", annotation);
	check_API_parameter_string(*comp_id, API_ID_GRID_MGT_REG_GRID_VIA_COR, comp_comm_group_mgt_mgr->get_comm_group_of_local_comp(*comp_id, "C-Coupler code in register_cor_defined_grid for getting component management node"), "registering a grid", CoR_grid_name, "CoR_grid_name", annotation);
	*grid_id = original_grid_mgr->get_CoR_defined_grid(*comp_id, CCPL_grid_name, CoR_grid_name, annotation);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish registering a CoR grid %s", CCPL_grid_name);
}


extern "C" void register_mid_point_grid_(int *level_3D_grid_id, int *mid_3D_grid_id, int *mid_1D_grid_id, int *size_mask, int *mask, const char *annotation)
{
	char API_label[NAME_STR_SIZE];


	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to register a middle level grid");

	get_API_hint(-1, API_ID_GRID_MGT_REG_MID_POINT_GRID, API_label);	
	check_for_ccpl_managers_allocated(API_ID_GRID_MGT_REG_MID_POINT_GRID, annotation);
	EXECUTION_REPORT(REPORT_ERROR, -1, original_grid_mgr->is_grid_id_legal(*level_3D_grid_id), "Error happens when calling API \"%s\" to register the mid-point grid of a grid: the grid ID of the interface-level grid (level_3D_grid_id) is wrong. Please verify the model code with the annotation \"%s.", API_label, annotation);
	check_for_coupling_registration_stage(original_grid_mgr->get_comp_id_of_grid(*level_3D_grid_id), API_ID_GRID_MGT_REG_MID_POINT_GRID, true, annotation);
	original_grid_mgr->register_mid_point_grid(*level_3D_grid_id, mid_3D_grid_id, mid_1D_grid_id, *size_mask, mask, annotation, API_label);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to register a middle level grid");
}


extern "C" void get_grid_size_(int *grid_id, int *grid_size, const char *annotation)
{
	check_for_ccpl_managers_allocated(API_ID_GRID_MGT_GET_GRID_SIZE, annotation);

	*grid_size = original_grid_mgr->get_grid_size(*grid_id, annotation);
}


extern "C" void get_grid_id_(int *comp_id, const char *grid_name, int *grid_id, const char *annotation)
{
	check_for_ccpl_managers_allocated(API_ID_GRID_MGT_GET_GRID_ID, annotation);

	*grid_id = original_grid_mgr->get_grid_id(*comp_id, grid_name, annotation);
}


extern "C" void get_h2d_grid_data_(int *grid_id, int *decomp_id, const char *label, const char *data_type, int *array_size, char *grid_data, const char *annotation)
{
	char API_label[NAME_STR_SIZE];


	check_for_ccpl_managers_allocated(API_ID_GRID_MGT_GET_H2D_GRID_DATA, annotation);
	get_API_hint(-1, API_ID_GRID_MGT_GET_H2D_GRID_DATA, API_label);
	EXECUTION_REPORT(REPORT_ERROR, -1, original_grid_mgr->is_grid_id_legal(*grid_id), "Error happens when calling API \"%s\" to get the grid data of an H2D grid: the parameter of \"grid_id\" is wrong. Please verify the model code with the annotation \"%s.", API_label, annotation);
	EXECUTION_REPORT(REPORT_ERROR, original_grid_mgr->get_comp_id_of_grid(*grid_id), original_grid_mgr->get_original_grid(*grid_id)->is_H2D_grid(), "Error happens when calling API \"%s\" to get the grid data of an H2D grid: the grid \"%s\" is not an H2D grid. Please verify the model code with the annotation \"%s.", API_label, original_grid_mgr->get_original_grid(*grid_id)->get_grid_name(), annotation);
	EXECUTION_REPORT(REPORT_ERROR, original_grid_mgr->get_comp_id_of_grid(*grid_id), *decomp_id == -1 || decomps_info_mgr->is_decomp_id_legal(*decomp_id), "Error happens when calling API \"%s\" to get the grid data of an H2D grid: the decomp_id is wrong (must be -1 or a legal decomp_id). Please verify the model code with the annotation \"%s.", API_label, annotation);
	if (*decomp_id != -1)
		EXECUTION_REPORT(REPORT_ERROR, original_grid_mgr->get_comp_id_of_grid(*grid_id), original_grid_mgr->get_comp_id_of_grid(*grid_id) == decomps_info_mgr->get_decomp_info(*decomp_id)->get_comp_id(), "Error happens when calling API \"%s\" to get the grid data of an H2D grid: the grid_id and decomp_id do not correspond to the same component model. Please verify the model code with the annotation \"%s.", API_label, annotation);
	original_grid_mgr->get_original_grid(*grid_id)->get_grid_data(*decomp_id, label, data_type, *array_size, grid_data, annotation, API_label);
}


extern "C" void register_parallel_decomposition_(int *decomp_id, int *grid_id, int *num_local_cells, int *array_size, const int *local_cells_global_indx, const char *decomp_name, const char *annotation)
{
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to register a parallel decomp %s", decomp_name);
	
	check_for_ccpl_managers_allocated(API_ID_DECOMP_MGT_REG_DECOMP, annotation);
	EXECUTION_REPORT(REPORT_ERROR, -1, original_grid_mgr->is_grid_id_legal(*grid_id), "Error happens when calling API \"CCPL_register_parallel_decomp\" to register a parallel decomposition \"%s\": the parameter \"grid_id\" is wrong. Please check the model code with the annotation \"%s\"", decomp_name, annotation);
	int comp_id = original_grid_mgr->get_comp_id_of_grid(*grid_id);
	check_for_coupling_registration_stage(comp_id, API_ID_DECOMP_MGT_REG_DECOMP, true, annotation);
	check_and_verify_name_format_of_string_for_API(comp_id, decomp_name, API_ID_DECOMP_MGT_REG_DECOMP, "the parallel decomposition", annotation);

	EXECUTION_REPORT(REPORT_ERROR, comp_id, original_grid_mgr->get_original_grid(*grid_id)->is_H2D_grid(), "Error happens when calling API \"CCPL_register_parallel_decomp\" to register a parallel decomposition \"%s\": the grid \"%s\" corresponding to the parameter \"grid_id\" is not a horizontal grid. Please check the model code with the annotation \"%s\"", decomp_name, original_grid_mgr->get_original_grid(*grid_id)->get_grid_name(), annotation);
	EXECUTION_REPORT(REPORT_ERROR, comp_id, *num_local_cells >= 0, "Error happens when calling API \"CCPL_register_parallel_decomp\" to register a parallel decomposition \"%s\": the parameter \"num_local_cells\" cannot be smaller than 0. Please check the model code with the annotation \"%s\"", decomp_name, annotation);
	EXECUTION_REPORT(REPORT_ERROR, comp_id, *num_local_cells <= *array_size, "Error happens when calling API \"CCPL_register_parallel_decomp\" to register a parallel decomposition \"%s\": the array size of the parameter \"local_cells_global_index\" cannot be smaller than the parameter \"num_local_cells\". Please check the model code with the annotation \"%s\"", decomp_name, annotation);
	int grid_size = original_grid_mgr->get_original_grid(*grid_id)->get_original_CoR_grid()->get_grid_size();
	for (int i = 0; i < *num_local_cells; i ++)
		if (local_cells_global_indx[i] != CCPL_NULL_INT)
			EXECUTION_REPORT(REPORT_ERROR, comp_id, local_cells_global_indx[i] > 0 && local_cells_global_indx[i] <= grid_size, "Error happens when calling API \"CCPL_register_parallel_decomp\" to register a parallel decomposition \"%s\": some values in parameter \"local_cells_global_indx\" are not between 1 and the size of the grid. Please check the model code with the annotation \"%s\"", decomp_name, annotation);
	*decomp_id = decomps_info_mgr->register_H2D_parallel_decomposition(decomp_name, *grid_id, *num_local_cells, local_cells_global_indx, annotation);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish registering a parallel decomp %s", decomp_name);
}


extern "C" void register_external_field_instance_(int *field_instance_id, const char *field_name, void *data_buffer, int *field_size, int *decomp_id, int *comp_or_grid_id, 
	                                             int *buf_mark, const char *unit, const char *data_type, const char *annotation)
{
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to register a field instance %s", field_name);

	check_for_ccpl_managers_allocated(API_ID_FIELD_MGT_REG_FIELD_INST, annotation);
	*field_instance_id = memory_manager->register_external_field_instance(field_name, data_buffer, *field_size, *decomp_id, *comp_or_grid_id, *buf_mark, unit, data_type, annotation);
	
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish registering a field instance %s", field_name);
}


extern "C" void register_an_io_field_from_field_instance_(int *field_inst_id, const char *field_IO_name, const char *annotation)
{
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to register an I/O field %s", field_IO_name);

	check_for_ccpl_managers_allocated(API_ID_FIELD_MGT_REG_IO_FIELD_from_INST, annotation);
	IO_fields_mgr->register_IO_field(*field_inst_id, field_IO_name, annotation);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish registering an I/O field %s", field_IO_name);
}


extern "C" void register_io_fields_from_field_instances_(int *num_field_inst, int *size_field_inst_ids, int *field_inst_ids, const char *annotation)
{
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to register I/O fields");

	check_for_ccpl_managers_allocated(API_ID_FIELD_MGT_REG_IO_FIELDs_from_INSTs, annotation);
	EXECUTION_REPORT(REPORT_ERROR, -1, *num_field_inst > 0, "Error happers when calling API \"CCPL_register_IO_fields_from_field_instances\": the parameter \"num_field_inst\" must be larger than 0. Please check the model code with the annotation \"%s\".", annotation);
	EXECUTION_REPORT(REPORT_ERROR, -1, *size_field_inst_ids > 0, "Error happers when calling API \"CCPL_register_IO_fields_from_field_instances\": the parameter \"field_inst_ids\" seems to be an empty array, which means it has not been allocated. Please check the model code with the annotation \"%s\".", annotation);
	IO_fields_mgr->register_IO_fields(*num_field_inst, *size_field_inst_ids, field_inst_ids, annotation);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish registering I/O fields");
}


extern "C" void register_a_new_io_field_(int *comp_or_grid_id, int *decomp_id, int *field_size, void *data_buffer, const char *field_IO_name, 
	                                    const char *long_name, const char *unit, const char *data_type, const char * annotation)
{
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to register an I/O field %s", field_IO_name);

	check_for_ccpl_managers_allocated(API_ID_FIELD_MGT_REG_IO_FIELD_from_BUFFER, annotation);
	IO_fields_mgr->register_IO_field(*comp_or_grid_id, *decomp_id, *field_size, data_buffer, field_IO_name, long_name, unit, data_type, annotation);
	
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish registering an I/O field %s", field_IO_name);
}


extern "C" void define_single_timer_(int *comp_id, int *timer_id, const char *freq_unit, int *freq_count, int *local_lag_count, int *remote_lag_count, const char *annotation)
{	
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to define a timer");
	
	check_for_coupling_registration_stage(*comp_id, API_ID_TIME_MGT_DEFINE_SINGLE_TIMER, true, annotation);
	EXECUTION_REPORT(REPORT_ERROR, *comp_id, components_time_mgrs->get_time_mgr(*comp_id)->get_time_step_in_second() > 0, "Error happers when calling API \"CCPL_define_single_timer\": the time step of the corresponding component has not been set yet. Please specify the time step before defining a timer at the model code with the annotation \"%s\"", 
		             comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id, annotation)->get_comp_name(), annotation);
	*timer_id = timer_mgr->define_timer(*comp_id, freq_unit, *freq_count, *local_lag_count, *remote_lag_count, annotation);
	
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish defining a timer");
}


extern "C" void define_complex_timer_(int *comp_id, int *timer_id, int *children_timers_id, int *num_children_timers, int *array_size, int *or_or_and, const char *annotation)
{
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to define a timer");

	check_for_coupling_registration_stage(*comp_id, API_ID_TIME_MGT_DEFINE_COMPLEX_TIMER, true, annotation);
	EXECUTION_REPORT(REPORT_ERROR, *comp_id, components_time_mgrs->get_time_mgr(*comp_id)->get_time_step_in_second() > 0, "The time step of the component \%s\" has not been set yet. Please specify the time step before defining a timer at the model code with the annotation \"%s\"", 
		             comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id, annotation)->get_comp_name(), annotation);
	*timer_id = timer_mgr->define_timer(*comp_id, children_timers_id, *num_children_timers, *array_size, *or_or_and, annotation);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish defining a timer");
}


extern "C" void set_component_time_step_(int *comp_id, int *time_step_in_second, const char *annotation)
{
	check_for_coupling_registration_stage(*comp_id, API_ID_TIME_MGT_SET_TIME_STEP, true, annotation);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to set the time step of component model \%s\"", comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id,"")->get_full_name());
		
	synchronize_comp_processes_for_API(*comp_id, API_ID_TIME_MGT_SET_TIME_STEP, comp_comm_group_mgt_mgr->get_comm_group_of_local_comp(*comp_id, "C-Coupler code in set_component_time_step_"), "setting the time step of a component", annotation);
	check_API_parameter_int(*comp_id, API_ID_TIME_MGT_SET_TIME_STEP, comp_comm_group_mgt_mgr->get_comm_group_of_local_comp(*comp_id,"C-Coupler code in set_component_time_step_"), NULL, *time_step_in_second, "time step (the unit is seconds)", annotation);
	components_time_mgrs->set_component_time_step(*comp_id, *time_step_in_second, annotation);
	
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finsh setting the time step of component model \%s\"", comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id,"")->get_full_name());
}


extern "C" void advance_component_time_(int *comp_id, const char *annotation)
{
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to advance time");
	
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_ADVANCE_TIME, annotation, false);
	EXECUTION_REPORT(REPORT_ERROR, *comp_id, comp_comm_group_mgt_mgr->get_is_definition_finalized(), "Error happens when calling API \"CCPL_advance_time\": the time of any component model cannot be advanced because the correponding root component model (\"%s\") has not called the API \"CCPL_end_coupling_configuration\" to finalize the stage of coupling configuration of the whole coupled model. Please verify the model code with the annotation \"%s\"", comp_comm_group_mgt_mgr->get_root_component_model()->get_comp_name(), annotation);
	components_IO_output_procedures_mgr->get_component_IO_output_procedures(*comp_id)->execute();
	components_time_mgrs->advance_component_time(*comp_id, annotation);
	EXECUTION_REPORT(REPORT_PROGRESS, *comp_id, true, "Component model \"%s\" advance time at the model code with the annotation \"%s\"", comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id,"")->get_full_name(), annotation);
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish advancing time");
}


extern "C" void ccpl_write_restart_(int *comp_id, int *bypass_timer, const char *annotation)
{
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to do restart write");
	check_for_component_registered(*comp_id, API_ID_RESTART_MGT_WRITE, annotation, false);
	EXECUTION_REPORT(REPORT_ERROR, *comp_id, comp_comm_group_mgt_mgr->get_is_definition_finalized(), "Error happens when calling API \"CCPL_do_restart_write\": the time of any component model cannot be advanced because the correponding root component model (\"%s\") has not called the API \"CCPL_end_coupling_configuration\" to finalize the stage of coupling configuration of the whole coupled model. Please verify the model code with the annotation \"%s\"", comp_comm_group_mgt_mgr->get_root_component_model()->get_comp_name(), annotation);
	if (comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id,annotation)->is_real_component_model())
		comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id,annotation)->get_restart_mgr()->do_restart_write(annotation, *bypass_timer == 1);
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish doing restart write");
}


extern "C" void ccpl_read_restart_(int *comp_id, const char *specified_file_name, const char *annotation)
{
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to do restart read");
	check_for_component_registered(*comp_id, API_ID_RESTART_MGT_READ, annotation, false);
	EXECUTION_REPORT(REPORT_ERROR, *comp_id, comp_comm_group_mgt_mgr->get_is_definition_finalized(), "Error happens when calling API \"CCPL_do_restart_write\": the time of any component model cannot be advanced because the correponding root component model (\"%s\") has not called the API \"CCPL_end_coupling_configuration\" to finalize the stage of coupling configuration of the whole coupled model. Please verify the model code with the annotation \"%s\"", comp_comm_group_mgt_mgr->get_root_component_model()->get_comp_name(), annotation);
	if (comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id,annotation)->is_real_component_model())
		comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id,annotation)->get_restart_mgr()->do_restart_read(specified_file_name, annotation);
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish doing restart read");
}


extern "C" void is_restart_timer_on_(int *comp_id, int *check_result, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_RESTART_MGT_IS_TIMER_ON, annotation, false);
	EXECUTION_REPORT(REPORT_ERROR, *comp_id, comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id,"in is_restart_timer_on_")->is_real_component_model(), "Error happens when calling the API CCPL_is_restart_timer_on: the given component model \"%s\" is not a real model. Please verify the model code related to the annotation \"%s\"",
		             comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id,"in is_restart_timer_on_")->get_comp_full_name(), annotation);
	if (components_time_mgrs->get_time_mgr(*comp_id)->is_restart_timer_on())
		*check_result = 1;
	else *check_result = 0;
}


extern "C" void check_ccpl_component_current_time_(int *comp_id, int *date, int *second, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_CHECK_CURRENT_TIME, annotation, false);
	components_time_mgrs->check_component_current_time(*comp_id, *date, *second, annotation);
}


extern "C" void is_ccpl_timer_on_(int *timer_id, int *is_on, const char *annotation)
{
	check_for_ccpl_managers_allocated(API_ID_TIME_MGT_IS_TIMER_ON, annotation);
	if (timer_mgr->is_timer_on(*timer_id, annotation))
		*is_on = 1;
	else *is_on = 0;
}


extern "C" void check_is_ccpl_model_run_ended_(int *comp_id, int *is_ended, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_TIME_MGT_IS_MODEL_RUN_ENDED, annotation, false);
	EXECUTION_REPORT(REPORT_ERROR, *comp_id, comp_comm_group_mgt_mgr->get_is_definition_finalized(), "Error happens when calling API \"CCPL_is_model_run_ended\": it cannot be called because the corresponding root component model (\"%s\") has not called the API \"CCPL_end_coupling_configuration\" to finalize the stage of coupling configuration of the whole coupled model. Please verify the model code with the annotation \"%s\"", comp_comm_group_mgt_mgr->get_root_component_model()->get_comp_name(), annotation);
	if (components_time_mgrs->is_model_run_ended(*comp_id, annotation))
		*is_ended = 1;
	else *is_ended = 0;
}


extern "C" void register_normal_remap_interface_(const char *interface_name, int *interface_id, int *num_fields, int *field_ids_src, int *field_ids_dst, int *timer_id, int *inst_or_aver, int *array_size1, int *array_size2, const char *annotation)
{
	char API_label[NAME_STR_SIZE];

	
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to register remap interface");
	check_for_ccpl_managers_allocated(API_ID_INTERFACE_REG_NORMAL_REMAP, annotation);	
	get_API_hint(-1, API_ID_INTERFACE_REG_NORMAL_REMAP, API_label);
	*interface_id = inout_interface_mgr->register_normal_remap_interface(interface_name, *num_fields, field_ids_src, field_ids_dst, *timer_id, *inst_or_aver, *array_size1, *array_size2, API_label, annotation);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish registering remap interface");
}


extern "C" void register_frac_based_remap_interface_(const char *interface_name, int *interface_id, int *num_fields, int *field_ids_src, int *field_ids_dst, int *timer_id, int *inst_or_aver, int *array_size1, int *array_size2, void *frac_src, void *frac_dst, int *size_frac_src, int *size_frac_dst, const char *frac_data_type, const char *annotation)
{
	char API_label[NAME_STR_SIZE];
	

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to register fraction based remap interface");

	check_for_ccpl_managers_allocated(API_ID_INTERFACE_REG_FRAC_REMAP, annotation);	
	get_API_hint(-1, API_ID_INTERFACE_REG_FRAC_REMAP, API_label);
	*interface_id = inout_interface_mgr->register_frac_based_remap_interface(interface_name, *num_fields, field_ids_src, field_ids_dst, *timer_id, *inst_or_aver, *array_size1, *array_size2, frac_src, frac_dst, *size_frac_src, *size_frac_dst, frac_data_type, API_label, annotation);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish registering fraction based remap interface");
}


extern "C" void register_inout_interface_(const char *interface_name, int *interface_id, int *import_or_export, int *num_fields, int *field_ids, int *timer_id, int *inst_or_aver, const char *interface_tag, const char *annotation, int *array_size1)
{
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to register import/export interface");

	if (*import_or_export == 0) {
		check_for_ccpl_managers_allocated(API_ID_INTERFACE_REG_IMPORT, annotation);
		*interface_id = inout_interface_mgr->register_inout_interface(interface_name, *import_or_export, *num_fields, field_ids, *array_size1, *timer_id, *inst_or_aver, interface_tag, annotation, INTERFACE_TYPE_REGISTER);
	}
	else {
		check_for_ccpl_managers_allocated(API_ID_INTERFACE_REG_EXPORT, annotation);
		*interface_id = inout_interface_mgr->register_inout_interface(interface_name, *import_or_export, *num_fields, field_ids, *array_size1, *timer_id, 0, interface_tag, annotation, INTERFACE_TYPE_REGISTER);
	}	

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish register import/export interface");
}


extern "C" void execute_inout_interface_with_id_(int *interface_id, int *bypass_timer, int *field_update_status, int *size_field_update_status, int *num_dst_fields, const char *annotation)
{
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to execute an interface");

	check_for_ccpl_managers_allocated(API_ID_INTERFACE_EXECUTE, annotation);
	inout_interface_mgr->execute_interface(*interface_id, *bypass_timer == 1, field_update_status, *size_field_update_status, num_dst_fields, annotation);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish executing an interface");
}


extern "C" void execute_inout_interface_with_name_(int *comp_id, const char *interface_name, int *bypass_timer, int *field_update_status, int *size_field_update_status, int *num_dst_fields, const char *annotation)
{
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to execute an interface");

	check_for_ccpl_managers_allocated(API_ID_INTERFACE_EXECUTE, annotation);
	inout_interface_mgr->execute_interface(*comp_id, interface_name, *bypass_timer == 1, field_update_status, *size_field_update_status, num_dst_fields, annotation);

	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish executing an interface");
}


extern "C" void connect_fixed_interfaces_between_two_components_(const char *comp_full_name1, const char *comp_full_name2, const char *annotation)
{
	const char *comp_full_name_low, *comp_full_name_high;
	Comp_comm_group_mgt_node *comp_node_low, *comp_node_high, *comp_node_low_pesudo = NULL, *comp_node_high_pesudo = NULL;


	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Start to connect two component models");

	check_for_ccpl_managers_allocated(API_ID_INTERFACE_CONNECT_INTERFACES, annotation);
	
	if (strcmp(comp_full_name1, comp_full_name2) < 0) {
		comp_full_name_low = comp_full_name1;
		comp_full_name_high = comp_full_name2;
	}
	else {
		comp_full_name_low = comp_full_name2;
		comp_full_name_high = comp_full_name1;
	}
		
	comp_node_low = comp_comm_group_mgt_mgr->search_global_node(comp_full_name_low);
	comp_node_high = comp_comm_group_mgt_mgr->search_global_node(comp_full_name_high);
	EXECUTION_REPORT(REPORT_ERROR, -1, (comp_node_low != NULL && comp_node_low->get_current_proc_local_id() >= 0) || (comp_node_high != NULL && comp_node_high->get_current_proc_local_id() >= 0),  "Error happens when calling API \"CCPL_connect_fixed_interfaces\" to connect the fixed interfaces between two component models \"%s\" and \"%s\": the full name of these two component models are wrong or the current process is not in both of these two component models. Please verify.", comp_full_name1, comp_full_name2);
	if (comp_node_low != NULL && comp_node_low->get_current_proc_local_id() >= 0)
		EXECUTION_REPORT(REPORT_ERROR, comp_node_low->get_comp_id(), !comp_node_low->is_definition_finalized(), "Error happens when calling API \"CCPL_connect_fixed_interfaces\" to connect the fixed interfaces between two component models \"%s\" and \"%s\": the coupling definition stage of \"%s\" has been finalized. Please check the model code at the annotation \"%s\".", comp_full_name1, comp_full_name2, comp_full_name_low, annotation);
	if (comp_node_high != NULL && comp_node_high->get_current_proc_local_id() >= 0)
		EXECUTION_REPORT(REPORT_ERROR, comp_node_high->get_comp_id(), !comp_node_high->is_definition_finalized(), "Error happens when calling API \"CCPL_connect_fixed_interfaces\" to connect the fixed interfaces between two component models \"%s\" and \"%s\": the coupling definition stage of \"%s\" has been finalized. Please check the model code at the annotation \"%s\".", comp_full_name1, comp_full_name2, comp_full_name_high, annotation);
	
	if (comp_node_low != NULL && comp_node_low->get_current_proc_local_id() >= 0) {
		if (comp_node_low->get_current_proc_local_id() == 0)
			EXECUTION_REPORT_LOG(REPORT_LOG, comp_node_low->get_comp_id(), true, "The whole component model \"%s\" is waiting to load the information of the component model \"%s\" at the model code with the annotation \"%s\"", comp_full_name_low, comp_full_name_high, annotation);
		comp_node_high_pesudo = comp_node_low->load_comp_info_from_XML(comp_full_name_high);
		if (comp_node_low->get_current_proc_local_id() == 0)
			EXECUTION_REPORT_LOG(REPORT_LOG, comp_node_low->get_comp_id(), true, "The whole component model \"%s\" successfully load the information of the component model \"%s\" at the model code with the annotation \"%s\"", comp_full_name_low, comp_full_name_high, annotation);
	}
	if (comp_node_high != NULL && comp_node_high->get_current_proc_local_id() >= 0) {
		if (comp_node_high->get_current_proc_local_id() == 0)
			EXECUTION_REPORT_LOG(REPORT_LOG, comp_node_high->get_comp_id(), true, "The whole component model \"%s\" is waiting to load the information of the component model \"%s\" at the model code with the annotation \"%s\"", comp_full_name_high, comp_full_name_low, annotation);
		comp_node_low_pesudo = comp_node_high->load_comp_info_from_XML(comp_full_name_low);
		if (comp_node_high->get_current_proc_local_id() == 0)
			EXECUTION_REPORT_LOG(REPORT_LOG, comp_node_high->get_comp_id(), true, "The whole component model \"%s\" successfully load the information of the component model \"%s\" at the model code with the annotation \"%s\"", comp_full_name_high, comp_full_name_low, annotation);
	}
	if (comp_node_high == NULL) {
		comp_node_high = comp_node_high_pesudo;
		comp_comm_group_mgt_mgr->push_comp_node(comp_node_high_pesudo);
	}
	if (comp_node_low == NULL) {
		comp_node_low = comp_node_low_pesudo;
		comp_comm_group_mgt_mgr->push_comp_node(comp_node_low_pesudo);
	}

	coupling_generator->connect_fixed_interfaces_between_two_components(comp_node_high, comp_node_low, annotation);

	if (comp_node_high == comp_node_high_pesudo) {
		bool check = comp_node_high_pesudo == comp_comm_group_mgt_mgr->pop_comp_node();
		EXECUTION_REPORT(REPORT_ERROR, -1, check, "Software error in connect_fixed_interfaces_between_two_components_");
	}

	if (comp_node_low == comp_node_low_pesudo) {
		bool check = comp_node_low_pesudo == comp_comm_group_mgt_mgr->pop_comp_node();
		EXECUTION_REPORT(REPORT_ERROR, -1, check, "Software error in connect_fixed_interfaces_between_two_components_");
	}

	if (comp_node_high_pesudo != NULL)
		delete comp_node_high_pesudo;
	if (comp_node_low_pesudo != NULL)
		delete comp_node_low_pesudo;
	
	EXECUTION_REPORT_LOG(REPORT_LOG, -1, true, "Finish connecting two component models");
}


extern "C" void get_comp_name_via_interface_tag_(int *comp_id, const char *interface_tag, char *comp_full_name, int *result, int *comp_full_name_size, const char *annotation)
{
	char local_comp_full_name[NAME_STR_SIZE], local_interface_name[NAME_STR_SIZE];

	
	check_for_component_registered(*comp_id, API_ID_INTERFACE_GET_COMP_NAME_VIA_TAG, annotation, false);
	if (comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id, "in get_comp_name_via_interface_tag_")->search_coupling_interface_tag(interface_tag, local_comp_full_name, local_interface_name))
		*result = 1;
	else *result = 0;
	if (*result == 1) {
		EXECUTION_REPORT(REPORT_ERROR, *comp_id, *comp_full_name_size >= strlen(local_comp_full_name), "Error happens when calling the API \"CCPL_get_comp_name_via_interface_tag\": the parameter string \"comp_full_name\" is too short: only %d while the size of the component model full name is %d", *comp_full_name_size, strlen(local_comp_full_name));
		strncpy(comp_full_name, local_comp_full_name, strlen(local_comp_full_name));
	}	
}


extern "C" void get_local_comp_full_name_(int *comp_id, char *comp_full_name, int *comp_full_name_size, const char *annotation)
{
	check_for_component_registered(*comp_id, API_ID_INTERFACE_GET_LOCAL_COMP_FULL_NAME, annotation, false);
	const char *full_name = comp_comm_group_mgt_mgr->get_global_node_of_local_comp(*comp_id, "in get_local_comp_full_name_")->get_full_name();
	EXECUTION_REPORT(REPORT_ERROR, *comp_id, *comp_full_name_size >= strlen(full_name), "Error happens when calling the API \"CCPL_get_local_comp_full_name\": the parameter string \"comp_full_name\" is too short: only %d while the size of the component model full name is %d", *comp_full_name_size, strlen(full_name));
	strncpy(comp_full_name, full_name, strlen(full_name));
	for (int i = strlen(full_name); i < *comp_full_name_size; i ++)
		comp_full_name[i] = ' ';
}


extern "C" void ccpl_report_(int *report_type, int *comp_id, int *condition, const char *report_content, const char *annotation)
{
	int API_id;
	bool local_condition = *condition == 1? true : false;

	
	if (*report_type == REPORT_ERROR)
		API_id = API_ID_REPORT_ERROR;
	else if (*report_type == REPORT_EXTERNAL_LOG)
		API_id = API_ID_REPORT_LOG;
	else API_id = API_ID_REPORT_PROGRESS;

	check_for_ccpl_managers_allocated(API_id, annotation);
	EXECUTION_REPORT(*report_type, *comp_id, local_condition, report_content);
}

