spmd_dyn.o spmd_dyn.d : spmd_dyn.F90
spmd_dyn.o : misc.h
spmd_dyn.o : params.h
spmd_dyn.o : shr_kind_mod.o
spmd_dyn.o : pmgrid.o
spmd_dyn.o : constituents.o
spmd_dyn.o : mpishorthand.o
spmd_dyn.o : infnan.o
spmd_dyn.o : pspect.o
spmd_dyn.o : comspe.o
