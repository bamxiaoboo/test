spmdinit.o spmdinit.d : spmdinit.F90
spmdinit.o : misc.h
spmdinit.o : params.h
spmdinit.o : mpishorthand.o
spmdinit.o : spmd_dyn.o
spmdinit.o : spmd_phys.o
spmdinit.o : pmgrid.o
spmdinit.o : commpi.h
