CC=gcc
FC=gfortran
CFLAGS=-c
FFLAGS=-O0 -g 
FFLAGS_OPT=-O2

all: spmv

spmv: get_walltime.o .FORCE
	$(FC) $(FFLAGS) spmv.f90 dummy.f90 get_walltime.o -o spmv

get_walltime.o:
	$(CC) $(CFLAGS) get_walltime.c

.FORCE:

clean:
	rm *.o spmv
