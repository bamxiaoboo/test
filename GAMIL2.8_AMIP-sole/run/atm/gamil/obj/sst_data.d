sst_data.o sst_data.d : sst_data.F90
sst_data.o : misc.h
sst_data.o : params.h
sst_data.o : shr_kind_mod.o
sst_data.o : pmgrid.o
sst_data.o : ppgrid.o
sst_data.o : phys_grid.o
sst_data.o : comsrf.o
sst_data.o : physconst.o
sst_data.o : commap.o
sst_data.o : rgrid.o
sst_data.o : error_messages.o
sst_data.o : time_manager.o
sst_data.o : mpishorthand.o
sst_data.o : comctl.h
sst_data.o : comlun.h
