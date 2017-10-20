ramp_ghg.o ramp_ghg.d : ramp_ghg.F90
ramp_ghg.o : misc.h
ramp_ghg.o : params.h
ramp_ghg.o : shr_kind_mod.o
ramp_ghg.o : pmgrid.o
ramp_ghg.o : ramp.h
ramp_ghg.o : physconst.o
ramp_ghg.o : constituents.o
ramp_ghg.o : time_manager.o
ramp_ghg.o : crdcon.h
ramp_ghg.o : ramp_ghg_bau.h
