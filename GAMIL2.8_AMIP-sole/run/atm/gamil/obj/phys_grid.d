phys_grid.o phys_grid.d : phys_grid.F90
phys_grid.o : misc.h
phys_grid.o : shr_kind_mod.o
phys_grid.o : ppgrid.o
phys_grid.o : pmgrid.o
phys_grid.o : spmd_dyn.o
phys_grid.o : mpishorthand.o
phys_grid.o : pspect.o
phys_grid.o : rgrid.o
phys_grid.o : commap.o
phys_grid.o : dyn_grid.o
