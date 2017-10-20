dyn_grid.o dyn_grid.d : dyn_grid.F90
dyn_grid.o : misc.h
dyn_grid.o : shr_kind_mod.o
dyn_grid.o : pmgrid.o
dyn_grid.o : rgrid.o
dyn_grid.o : spmd_dyn.o
