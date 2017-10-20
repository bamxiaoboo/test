phys_idealized.o phys_idealized.d : phys_idealized.F90
phys_idealized.o : misc.h
phys_idealized.o : params.h
phys_idealized.o : shr_kind_mod.o
phys_idealized.o : ppgrid.o
phys_idealized.o : phys_grid.o
phys_idealized.o : physics_types.o
phys_idealized.o : buffer.o
phys_idealized.o : comsrf.o
phys_idealized.o : diagnostics.o
phys_idealized.o : geopotential.o
phys_idealized.o : physconst.o
