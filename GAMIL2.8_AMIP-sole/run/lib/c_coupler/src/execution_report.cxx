/***************************************************************
  *  Copyright (c) 2017, Tsinghua University.
  *  This is a source file of C-Coupler.
  *  This file was initially finished by Dr. Li Liu. 
  *  If you have any problem, 
  *  please contact Dr. Li Liu via liuli-cess@tsinghua.edu.cn
  ***************************************************************/


#ifndef ONLY_CoR
#include "global_data.h"
#else
#define NAME_STR_SIZE 1024
#endif
#include "execution_report.h"
#include "cor_global_data.h"
#include <assert.h>
#include <stdio.h>
#include <sys/time.h>


bool report_external_log_enabled;
bool report_error_enabled;
bool report_progress_enabled;
bool report_internal_log_enabled;


void import_report_setting()
{
	char XML_file_name[NAME_STR_SIZE];
	int line_number;
	char keywords[4][NAME_STR_SIZE];
	bool report_setting[4];


	report_external_log_enabled = false;
	report_error_enabled = false;
	report_internal_log_enabled = false;
	report_progress_enabled = false;

	sprintf(XML_file_name, "%s/all/CCPL_report.xml", comp_comm_group_mgt_mgr->get_config_root_dir());
	TiXmlDocument *XML_file = open_XML_file_to_read(-1, XML_file_name, MPI_COMM_WORLD, false);
	if (XML_file == NULL)
		return;

	sprintf(keywords[0], "report_internal_log");
	sprintf(keywords[1], "report_external_log");
	sprintf(keywords[2], "report_progress");
	sprintf(keywords[3], "report_error");
	
	TiXmlElement *XML_element = XML_file->FirstChildElement();
	for (int i = 0; i < 4; i ++) {
		report_setting[i] = false;
		const char *setting = XML_element->Attribute(keywords[i], &line_number);
		if (setting == NULL)
			continue;
		EXECUTION_REPORT(REPORT_ERROR, -1, words_are_the_same(setting, "on") || words_are_the_same(setting, "off"), "Error happens when loading the XML file \"%s\": the value of \"%s\" must be \"on\" or \"off\". Please verify the XML file around line number %d", XML_file_name, keywords[i]);
		report_setting[i] = words_are_the_same(setting,"on");
	}

	delete XML_file;

	report_internal_log_enabled = report_setting[0];
	report_external_log_enabled = report_setting[1];
	report_progress_enabled = report_setting[2];
	report_error_enabled = report_setting[3];
}


void wtime(double *t)
{
  static int sec = -1;

  struct timeval tv;
  gettimeofday(&tv, NULL);
  if (sec < 0) 
    sec = tv.tv_sec;
  *t = (tv.tv_sec - sec) + 1.0e-6*tv.tv_usec;
}


void report_header(int report_type, int comp_id, bool &condition, char *output_string)
{
	if (comp_id != -1 && (comp_id == comp_comm_group_mgt_mgr->get_global_node_root()->get_comp_id() || !comp_comm_group_mgt_mgr->is_legal_local_comp_id(comp_id) || components_time_mgrs->get_time_mgr(comp_id) == NULL))
		comp_id = -1;
	
	output_string[0] = '\0';
	
	switch (report_type) {
		case REPORT_ERROR:
			condition = !condition;
			break;
		case REPORT_LOG:
			condition = report_internal_log_enabled;
			break;
		case REPORT_EXTERNAL_LOG:
			condition = report_external_log_enabled;
			break;
		case REPORT_WARNING:
			condition = !condition;
			break;
		case REPORT_PROGRESS:
			if (comp_id == -1)
				condition = comp_comm_group_mgt_mgr->get_current_proc_global_id() == 0 && report_progress_enabled;
			else condition = comp_comm_group_mgt_mgr->get_global_node_of_local_comp(comp_id,"")->get_current_proc_local_id() == 0 && report_progress_enabled;
			break;
		default:
			printf("report type %d is not support\n", report_type);
			assert(false);
	}

	if (!condition)
		return;

	switch (report_type) {
		case REPORT_ERROR:
			if (line_number > 0 && execution_phase_number < 2)
				sprintf(output_string+strlen(output_string), "CoR REPORT ERROR at script line %d: ", line_number);
#ifndef ONLY_CoR
			else sprintf(output_string+strlen(output_string), "C-Coupler REPORT ERROR: ");
#endif
			break;
		case REPORT_LOG:
			if (line_number > 0 && execution_phase_number < 2)
				sprintf(output_string+strlen(output_string), "CoR REPORT LOG at script line %d: ", line_number);
#ifndef ONLY_CoR
			else sprintf(output_string+strlen(output_string), "C-Coupler REPORT LOG: ");
#endif			
			break;
			case REPORT_EXTERNAL_LOG:
				if (line_number > 0 && execution_phase_number < 2)
					sprintf(output_string+strlen(output_string), "CoR REPORT LOG at script line %d: ", line_number);
#ifndef ONLY_CoR
				else sprintf(output_string+strlen(output_string), "C-Coupler REPORT LOG: ");
#endif			
				break;
		case REPORT_WARNING:
			if (line_number > 0 && execution_phase_number < 2)
				sprintf(output_string+strlen(output_string), "CoR REPORT WARNING at script line %d: ", line_number);
#ifndef ONLY_CoR
			else sprintf(output_string+strlen(output_string), "C-Coupler REPORT WARNING: ");
#endif
			break;
		case REPORT_PROGRESS:
			if (line_number > 0 && execution_phase_number < 2)
				sprintf(output_string+strlen(output_string), "CoR REPORT PROGRESS at script line %d: ", line_number);
#ifndef ONLY_CoR
 			else sprintf(output_string+strlen(output_string), "C-Coupler REPORT PROGRESS: ");
#endif
			break;
		default:
			printf("Software error: report type %d is not supported\n", report_type);
			assert(false);
			break;
	}
#ifndef ONLY_CoR
	if (!(line_number > 0 && execution_phase_number < 2) && comp_comm_group_mgt_mgr != NULL) {
		if (comp_id == -1)
			sprintf(output_string+strlen(output_string)-2, " in the root component model corresponding to the executable named \"%s\": ", comp_comm_group_mgt_mgr->get_executable_name());
		else {
			int current_date = components_time_mgrs->get_time_mgr(comp_id)->get_current_date();
			int current_second = components_time_mgrs->get_time_mgr(comp_id)->get_current_second();
			int current_step_id = components_time_mgrs->get_time_mgr(comp_id)->get_current_step_id();
			sprintf(output_string+strlen(output_string)-2, " in the component model \"%s\" corresponding to the executable named \"%s\", at the current simulation time of %08d-%05d (the current step number is %d): ", 
				    comp_comm_group_mgt_mgr->get_global_node_of_local_comp(comp_id,"execution report")->get_full_name(), comp_comm_group_mgt_mgr->get_executable_name(), current_date, current_second, current_step_id);
		}
	}
#endif
}


void execution_report(int report_type, int comp_id, bool condition, const char *format, ...)
{
	char output_string[NAME_STR_SIZE*4];
	FILE *log_file;
	

	report_header(report_type, comp_id, condition, output_string);
	
	if (!condition)
		return;
	
	strcat(output_string, format);
	strcat(output_string, "\n\n");

	if (comp_id != -1 && (comp_id == comp_comm_group_mgt_mgr->get_global_node_root()->get_comp_id() || !comp_comm_group_mgt_mgr->is_legal_local_comp_id(comp_id) || components_time_mgrs->get_time_mgr(comp_id) == NULL))
		comp_id = -1;
	
	if (comp_comm_group_mgt_mgr == NULL || (comp_id == -1 && comp_comm_group_mgt_mgr->get_exe_log_file_name(comp_id) == NULL)) {
		va_list pArgList;
	    va_start(pArgList, format);
		vfprintf(stdout, output_string, pArgList);
		va_end(pArgList);
		fflush(stdout);
	}
	else {
		const char *log_file_name = comp_comm_group_mgt_mgr->get_exe_log_file_name(comp_id);
		FILE *log_file = fopen(log_file_name, "a+");
		va_list pArgList;
	    va_start(pArgList, format);
		vfprintf(log_file, output_string, pArgList);
		va_end(pArgList);
		fclose(log_file);
		if (comp_id != -1) {
			log_file_name = comp_comm_group_mgt_mgr->get_comp_log_file_name(comp_id);
			log_file = fopen(log_file_name, "a+");
			va_start(pArgList, format);
			vfprintf(log_file, output_string, pArgList);
			va_end(pArgList);
			fclose(log_file);
		}
		if (report_type == REPORT_ERROR)
			printf("ERROR happens. Please check the log file \"%s\"\n\n", log_file_name);
	}

	if (report_type == REPORT_ERROR)
		assert(false);
}



void execution_report(int report_type, int comp_id, bool condition) 
{
	char output_string[NAME_STR_SIZE*4];


	report_header(report_type, comp_id, condition, output_string);

	if (!condition)
		return;

	if (comp_id != -1 && (comp_id == comp_comm_group_mgt_mgr->get_global_node_root()->get_comp_id() || !comp_comm_group_mgt_mgr->is_legal_local_comp_id(comp_id) || components_time_mgrs->get_time_mgr(comp_id) == NULL))
		comp_id = -1;

	if (comp_comm_group_mgt_mgr == NULL || (comp_id == -1 && comp_comm_group_mgt_mgr->get_exe_log_file_name(comp_id) == NULL)) {
		printf("%s\n\n", output_string);
		fflush(NULL);
	}
	else {
		const char *log_file_name = comp_comm_group_mgt_mgr->get_exe_log_file_name(comp_id);
		FILE *log_file = fopen(log_file_name, "a+");
		fprintf(log_file, "%s\n\n", output_string);
		fclose(log_file);
		if (comp_id != -1) {
			log_file_name = comp_comm_group_mgt_mgr->get_comp_log_file_name(comp_id);
			log_file = fopen(log_file_name, "a+");
			fprintf(log_file, "%s\n\n", output_string);
			fclose(log_file);
		}
		if (report_type == REPORT_ERROR)
			printf("ERROR happens. Please check the log file \"%s\"\n\n", log_file_name);
	}

	if (report_type == REPORT_ERROR)
		assert(false);
}


