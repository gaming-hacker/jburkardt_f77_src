#! /bin/bash
#
cp ~/include/mpi_stubs_f77.h mpif.h
#
gfortran -c quad2_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
rm mpif.h
#
gfortran quad2_mpi.o -L$HOME/libf77 -lmpi_stubs
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm quad2_mpi.o
#
mv a.out ~/binf77/quad2_mpi_stubs
#
echo "Normal end of execution."
