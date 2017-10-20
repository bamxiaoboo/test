mkgridMod.o mkgridMod.d : mkgridMod.F90
mkgridMod.o : misc.h
mkgridMod.o : preproc.h
mkgridMod.o : shr_kind_mod.o
mkgridMod.o : clm_varpar.o
mkgridMod.o : clm_varsur.o
mkgridMod.o : clm_varctl.o
mkgridMod.o : fileutils.o
mkgridMod.o : areaMod.o
mkgridMod.o : spmdMod.o
