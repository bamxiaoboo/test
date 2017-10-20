time_manager.o time_manager.d : time_manager.F90
time_manager.o : misc.h
time_manager.o : shr_kind_mod.o
time_manager.o : pmgrid.o
time_manager.o : string_utils.o
time_manager.o : dycore.o
time_manager.o : mpishorthand.o
