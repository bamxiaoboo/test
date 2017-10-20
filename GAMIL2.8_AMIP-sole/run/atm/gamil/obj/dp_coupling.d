dp_coupling.o dp_coupling.d : dp_coupling.F90
dp_coupling.o : shr_kind_mod.o
dp_coupling.o : ppgrid.o
dp_coupling.o : rgrid.o
dp_coupling.o : pmgrid.o
dp_coupling.o : phys_grid.o
dp_coupling.o : physics_types.o
dp_coupling.o : constituents.o
dp_coupling.o : physconst.o
