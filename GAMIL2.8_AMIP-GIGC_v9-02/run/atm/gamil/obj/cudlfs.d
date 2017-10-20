cudlfs.o cudlfs.d : cudlfs.F90
cudlfs.o : shr_kind_mod.o
cudlfs.o : mo_constants.o
cudlfs.o : mo_cumulus_flux.o
