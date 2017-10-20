phys_adiabatic.o phys_adiabatic.d : phys_adiabatic.F90
phys_adiabatic.o : misc.h
phys_adiabatic.o : params.h
phys_adiabatic.o : shr_kind_mod.o
phys_adiabatic.o : ppgrid.o
phys_adiabatic.o : tracers.o
phys_adiabatic.o : phys_grid.o
phys_adiabatic.o : physics_types.o
phys_adiabatic.o : diagnostics.o
phys_adiabatic.o : geopotential.o
phys_adiabatic.o : physconst.o
