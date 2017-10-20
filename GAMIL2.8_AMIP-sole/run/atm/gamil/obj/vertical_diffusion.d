vertical_diffusion.o vertical_diffusion.d : vertical_diffusion.F90
vertical_diffusion.o : misc.h
vertical_diffusion.o : params.h
vertical_diffusion.o : shr_kind_mod.o
vertical_diffusion.o : ppgrid.o
vertical_diffusion.o : constituents.o
vertical_diffusion.o : tracers.o
vertical_diffusion.o : pmgrid.o
vertical_diffusion.o : phys_buffer.o
vertical_diffusion.o : history.o
vertical_diffusion.o : turbulence.o
vertical_diffusion.o : physics_types.o
vertical_diffusion.o : phys_grid.o
vertical_diffusion.o : commap.o
