#!/bin/bash
#
cp ~/include/mpi_stubs_f77.h mpif.h
#
gfortran -c buffon_laplace.f
if [ $? -ne 0 ]; then
  echo "Errors occurred while compiling buffon_laplace.f"
  exit
fi
rm mpif.h
#
gfortran buffon_laplace.o -L$HOME/libf77 -lmpi_stubs
if [ $? -ne 0 ]; then
  echo "Errors occurred while linking and loading buffon_laplace.o"
  exit
fi
rm buffon_laplace.o
#
mv a.out buffon_laplace
./buffon_laplace > buffon_laplace_output.txt
if [ $? -ne 0 ]; then
  echo "Errors occurred while running buffon_laplace"
  exit
fi
rm buffon_laplace
#
echo "Program output written to buffon_laplace_output.txt"
