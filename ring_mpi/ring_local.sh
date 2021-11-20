#! /bin/bash
#
mpifort -c -Wall ring_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort ring_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ring_mpi.o
mv a.out ring
#
mpirun -np 8 ./ring > ring.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ring
#
echo "Normal end of execution."

