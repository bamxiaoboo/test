phys_buffer.o phys_buffer.d : phys_buffer.F90
phys_buffer.o : misc.h
phys_buffer.o : shr_kind_mod.o
phys_buffer.o : infnan.o
phys_buffer.o : pmgrid.o
phys_buffer.o : ppgrid.o
phys_buffer.o : phys_grid.o
phys_buffer.o : dyn_grid.o
phys_buffer.o : string_utils.o
phys_buffer.o : mpishorthand.o
