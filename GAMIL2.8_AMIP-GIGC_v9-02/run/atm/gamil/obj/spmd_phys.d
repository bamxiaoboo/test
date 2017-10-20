spmd_phys.o spmd_phys.d : spmd_phys.F90
spmd_phys.o : misc.h
spmd_phys.o : params.h
spmd_phys.o : shr_kind_mod.o
spmd_phys.o : pmgrid.o
spmd_phys.o : ppgrid.o
