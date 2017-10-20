program_csm.o program_csm.d : program_csm.F90
program_csm.o : misc.h
program_csm.o : preproc.h
program_csm.o : shr_kind_mod.o
program_csm.o : clm_varpar.o
program_csm.o : clm_varctl.o
program_csm.o : shr_orb_mod.o
program_csm.o : shr_msg_mod.o
program_csm.o : spmdMod.o
program_csm.o : initializeMod.o
program_csm.o : clm_csmMod.o
program_csm.o : time_manager.o
program_csm.o : gpt.inc
