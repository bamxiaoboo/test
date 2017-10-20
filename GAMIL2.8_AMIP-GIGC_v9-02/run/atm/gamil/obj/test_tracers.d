test_tracers.o test_tracers.d : test_tracers.F90
test_tracers.o : misc.h
test_tracers.o : params.h
test_tracers.o : shr_kind_mod.o
test_tracers.o : pmgrid.o
test_tracers.o : tracers.o
test_tracers.o : comctl.h
test_tracers.o : ppgrid.o
test_tracers.o : phys_grid.o
