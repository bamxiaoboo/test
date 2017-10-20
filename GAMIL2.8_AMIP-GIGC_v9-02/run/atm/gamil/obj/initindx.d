initindx.o initindx.d : initindx.F90
initindx.o : shr_kind_mod.o
initindx.o : constituents.o
initindx.o : chemistry.o
initindx.o : physconst.o
initindx.o : tracers.o
initindx.o : phys_buffer.o
initindx.o : MG.o
initindx.o : vertical_diffusion.o
initindx.o : pmgrid.o
initindx.o : comctl.h
initindx.o : RK_or_MG.h
