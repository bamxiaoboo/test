pftcFileMod.o pftcFileMod.d : pftcFileMod.F90
pftcFileMod.o : misc.h
pftcFileMod.o : preproc.h
pftcFileMod.o : shr_kind_mod.o
pftcFileMod.o : clm_varpar.o
pftcFileMod.o : clm_varctl.o
pftcFileMod.o : pft_varcon.o
pftcFileMod.o : fileutils.o
pftcFileMod.o : spmdMod.o
