trcmix.o trcmix.d : trcmix.F90
trcmix.o : misc.h
trcmix.o : params.h
trcmix.o : shr_kind_mod.o
trcmix.o : ppgrid.o
trcmix.o : phys_grid.o
trcmix.o : physconst.o
trcmix.o : constituents.o
