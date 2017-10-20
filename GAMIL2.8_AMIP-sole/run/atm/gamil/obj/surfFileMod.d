surfFileMod.o surfFileMod.d : surfFileMod.F90
surfFileMod.o : misc.h
surfFileMod.o : preproc.h
surfFileMod.o : shr_kind_mod.o
surfFileMod.o : clm_varpar.o
surfFileMod.o : clm_varctl.o
surfFileMod.o : clm_varsur.o
surfFileMod.o : pft_varcon.o
surfFileMod.o : fileutils.o
surfFileMod.o : spmdMod.o
surfFileMod.o : areaMod.o
