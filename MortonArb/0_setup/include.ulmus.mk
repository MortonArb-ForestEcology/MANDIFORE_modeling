# Makefile include include.mk.opt

# Define make (gnu make works best).
MAKE=/usr/bin/make

# libraries.
BASE=$(ED_ROOT)/build/

# HDF 5  Libraries
# ED2 HAS OPTIONAL HDF 5 I/O
# If you wish to use this functionality specify USE_HDF5=1
# and specify the location of the include directory
# library files. Make sure you include the zlib.a location too.

# For 64-bit
HDF5HOME=/usr/local/hdf5
USE_HDF5=1
HDF5_INCS=-I${HDF5HOME}/include
HDF5_LIBS=-lm -lz -L${HDF5HOME}/lib -lhdf5 -lhdf5_fortran -lhdf5_hl
USE_COLLECTIVE_MPIO=1
 
# netCDF libraries ---------------------------------------------
# If you have netCDF set USENC=1 and type the lib folder
# at NC_LIBS, with the leading -L (e.g. -L/usr/local/lib).
# If you don't have it, leave USENC=0 and type a dummy
# folder for NC_LIBS (e.g. -L/dev/null or leave it blank)
USENC=0
NC_LIBS=-L/dev/null

# interface ----------------------------------------------------
# This should be 1 unless you are running with -gen-interfaces.
# Interfaces usually make the compilation to crash when the
# -gen-interfaces option are on, so this flag bypass all
# interfaces in the code.
USE_INTERF=1

# MPI_Wtime. ---------------------------------------------------
# If USE_MPIWTIME=1, then it will use MPI libraries to compute
# the wall time (the only double-precision intrinsic).  In case
# you don't have it, leave USE_MPIWTIME=0, in which case it will
# use a simpler, single-precision function.
USE_MPIWTIME=1

# Compile flags ------------------------------------------------

CMACH=PC_LINUX1

F_COMP=mpif90
#F_COMP = gfortran

F_OPTS= -O3 -ffree-line-length-none -fno-whole-file -fopenmp

#F_OPTS= -g -Wall -W -ffpe-trap=invalid,zero,overflow -Wconversion -fbounds-check -fbacktrace -fdump-core
#F_OPTS= -V -FR -O2 -recursive -static -Vaxlib  -check all -g -fpe0 -ftz  -debug extended \
#        -debug inline_debug_info -debug-parameters all -traceback -ftrapuv
#F_Opts= -03

C_COMP=mpicc

#C_OPTS= -O2 -DLITTLE  -g -static -traceback -debug extended
C_OPTS = -O3 -DLITTLE

LOADER=mpif90
#LOADER = gfortran

LOADER_OPTS=${F_OPTS}

C_LOADER=mpicc
#C_LOADER_OPTS=-v -g -traceback -static

LIBS=
MOD_EXT=mod

# MPI Flags ----------------------------------------------------
MPI_PATH=
PAR_INCS=
PAR_LIBS=
PAR_DEFS=-DRAMS_MPI

# For IBM,HP,SGI,ALPHA,LINUX use these:
ARCHIVE=ar rs

