buffer.o buffer.d : buffer.F90
buffer.o : misc.h
buffer.o : params.h
buffer.o : shr_kind_mod.o
buffer.o : tracers.o
buffer.o : ppgrid.o
buffer.o : prognostics.o
buffer.o : infnan.o
