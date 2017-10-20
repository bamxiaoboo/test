mkpft.o mkpft.d : mkpft.F90
mkpft.o : misc.h
mkpft.o : preproc.h
mkpft.o : shr_kind_mod.o
mkpft.o : clm_varpar.o
mkpft.o : clm_varsur.o
mkpft.o : clm_varctl.o
mkpft.o : fileutils.o
mkpft.o : areaMod.o
mkpft.o : shr_sys_mod.o
