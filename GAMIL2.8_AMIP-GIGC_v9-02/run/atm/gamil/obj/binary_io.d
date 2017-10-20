binary_io.o binary_io.d : binary_io.F90
binary_io.o : misc.h
binary_io.o : params.h
binary_io.o : shr_kind_mod.o
binary_io.o : pmgrid.o
binary_io.o : spmd_dyn.o
binary_io.o : mpishorthand.o
