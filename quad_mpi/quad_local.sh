#! /bin/bash
#
mpifort -c -Wall quad_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort quad_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm quad_mpi.o
mv a.out quad
#
mpirun -np 8 ./quad > quad.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm quad
#
echo "Normal end of execution."

