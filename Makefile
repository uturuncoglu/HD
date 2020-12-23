FC = ifort -O3 -fp-model precise -fp-model source -DCPL
NETCDF=/usr/local
FCFLAGS = -I$(NETCDF)/include
CPPFLAGS = -L$(NETCDF)/lib -lnetcdf -lnetcdff
APP = hdmain.x

SRC = mod_hd_param.f90 \
      mod_hd_io.f90    \
      mod_hd_model.f90 \
      mod_hd_diags.f90 \
      mod_hd_iface.F90 \
      hdmain.f90

OBJ = $(SRC:.f90=.o)

$(APP): $(OBJ) mod_hd_iface.o 
	$(FC) -o $(APP) $(OBJ) $(CPPFLAGS)

%.o: %.F90
	$(FC) $(FCFLAGS) -c $<

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

%.o: %.f
	$(FC) $(FCFLAGS) -c $<

install: $(APP)

clean:
	rm -f $(APP) *.o *.mod

mod_hd_param.o: mod_hd_param.f90
mod_hd_io.o: mod_hd_io.f90 mod_hd_param.o
mod_hd_model.o: mod_hd_model.f90 mod_hd_param.o
mod_hd_diags.o: mod_hd_diags.f90 mod_hd_param.o
mod_hd_iface.o: mod_hd_iface.F90 mod_hd_param.o mod_hd_io.o mod_hd_model.o mod_hd_diags.o
hdmain.o: hdmain.f90 mod_hd_param.o mod_hd_iface.o
