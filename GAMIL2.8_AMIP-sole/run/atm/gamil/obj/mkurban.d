mkurban.o mkurban.d : mkurban.F90
mkurban.o : misc.h
mkurban.o : preproc.h
mkurban.o : shr_kind_mod.o
mkurban.o : clm_varpar.o
mkurban.o : clm_varsur.o
mkurban.o : clm_varctl.o
mkurban.o : fileutils.o
mkurban.o : areaMod.o
mkurban.o : shr_sys_mod.o
