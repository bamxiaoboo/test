turbulence.o turbulence.d : turbulence.F90
turbulence.o : misc.h
turbulence.o : params.h
turbulence.o : shr_kind_mod.o
turbulence.o : ppgrid.o
turbulence.o : pmgrid.o
turbulence.o : tracers.o
turbulence.o : history.o
turbulence.o : coupling_chemistry_model_mod.o
