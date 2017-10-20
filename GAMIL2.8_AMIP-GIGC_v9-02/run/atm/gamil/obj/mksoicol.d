mksoicol.o mksoicol.d : mksoicol.F90
mksoicol.o : misc.h
mksoicol.o : preproc.h
mksoicol.o : shr_kind_mod.o
mksoicol.o : clm_varpar.o
mksoicol.o : clm_varsur.o
mksoicol.o : clm_varctl.o
mksoicol.o : fileutils.o
mksoicol.o : areaMod.o
mksoicol.o : shr_sys_mod.o
