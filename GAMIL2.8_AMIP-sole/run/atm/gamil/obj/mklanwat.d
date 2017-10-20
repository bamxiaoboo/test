mklanwat.o mklanwat.d : mklanwat.F90
mklanwat.o : misc.h
mklanwat.o : preproc.h
mklanwat.o : shr_kind_mod.o
mklanwat.o : clm_varpar.o
mklanwat.o : clm_varsur.o
mklanwat.o : clm_varctl.o
mklanwat.o : fileutils.o
mklanwat.o : areaMod.o
mklanwat.o : shr_sys_mod.o
