diagnostics.o diagnostics.d : diagnostics.F90
diagnostics.o : misc.h
diagnostics.o : params.h
diagnostics.o : shr_kind_mod.o
diagnostics.o : ppgrid.o
diagnostics.o : history.o
diagnostics.o : constituents.o
diagnostics.o : physics_types.o
diagnostics.o : physconst.o
diagnostics.o : wv_saturation.o
diagnostics.o : coupling_chemistry_model_mod.o
diagnostics.o : ccsm_msg.o
diagnostics.o : time_manager.o
