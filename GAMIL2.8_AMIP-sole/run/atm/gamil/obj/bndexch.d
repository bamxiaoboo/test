bndexch.o bndexch.d : bndexch.F90
bndexch.o : misc.h
bndexch.o : params.h
bndexch.o : pmgrid.o
bndexch.o : spmd_dyn.o
bndexch.o : constituents.o
bndexch.o : prognostics.o
bndexch.o : mpishorthand.o
