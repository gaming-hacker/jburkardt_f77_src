#! /bin/bash
#
mpifort -c -Wall poisson_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort poisson_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm poisson_mpi.o
mv a.out poisson_band
#
mpirun -v -np 4 ./poisson_band > poisson_band.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm poisson_band
#
echo "Normal end of execution."
