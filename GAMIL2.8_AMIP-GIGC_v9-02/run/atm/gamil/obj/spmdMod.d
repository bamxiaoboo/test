spmdMod.o spmdMod.d : spmdMod.F90
spmdMod.o : misc.h
spmdMod.o : preproc.h
spmdMod.o : shr_msg_mod.o
spmdMod.o : mpishorthand.o
spmdMod.o : spmd_dyn.o
spmdMod.o : pmgrid.o
