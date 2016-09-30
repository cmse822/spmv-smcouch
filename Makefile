CC=gcc
FC=gfortran
CFLAGS=-c
FFLAGS=-O3 -g
FFLAGS_OPT=-O2

all: spmv spmv_2

spmv: get_walltime.o .FORCE
	$(FC) $(FFLAGS) spmv.f90 dummy.f90 get_walltime.o -o spmv

spmv_2: get_walltime.o .FORCE
		$(FC) $(FFLAGS) spmv_2.f90 dummy.f90 get_walltime.o -o spmv_2

spmv_new: get_walltime.o dummy.o .FORCE
		$(FC) $(FFLAGS) spmv_new.f90 dummy.o get_walltime.o -o spmv_new

dmv: get_walltime.o dummy.o .FORCE
		$(FC) $(FFLAGS) dmv.f90 dummy.o get_walltime.o -o dmv

dummy.o:
	  $(FC) $(FFLAGS) -c dummy.f90

get_walltime.o:
	$(CC) $(CFLAGS) get_walltime.c

.FORCE:

clean:
	rm *.o spmv
