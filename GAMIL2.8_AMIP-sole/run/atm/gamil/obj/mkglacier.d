mkglacier.o mkglacier.d : mkglacier.F90
mkglacier.o : misc.h
mkglacier.o : preproc.h
mkglacier.o : shr_kind_mod.o
mkglacier.o : clm_varpar.o
mkglacier.o : clm_varsur.o
mkglacier.o : clm_varctl.o
mkglacier.o : fileutils.o
mkglacier.o : areaMod.o
mkglacier.o : shr_sys_mod.o
