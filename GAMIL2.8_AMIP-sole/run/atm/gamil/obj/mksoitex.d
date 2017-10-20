mksoitex.o mksoitex.d : mksoitex.F90
mksoitex.o : misc.h
mksoitex.o : preproc.h
mksoitex.o : shr_kind_mod.o
mksoitex.o : clm_varpar.o
mksoitex.o : clm_varsur.o
mksoitex.o : clm_varctl.o
mksoitex.o : fileutils.o
mksoitex.o : areaMod.o
mksoitex.o : shr_sys_mod.o
