histFileMod.o histFileMod.d : histFileMod.F90
histFileMod.o : misc.h
histFileMod.o : preproc.h
histFileMod.o : shr_kind_mod.o
histFileMod.o : clmtype.o
histFileMod.o : clm_varpar.o
histFileMod.o : clm_varmap.o
histFileMod.o : shr_const_mod.o
histFileMod.o : fileutils.o
histFileMod.o : clm_varctl.o
histFileMod.o : spmdMod.o
histFileMod.o : clm_varsur.o
histFileMod.o : time_manager.o
histFileMod.o : clm_varder.o
histFileMod.o : mpishorthand.o
histFileMod.o : shr_sys_mod.o
histFileMod.o : infnan.o
