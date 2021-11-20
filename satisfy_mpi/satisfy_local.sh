#! /bin/bash
#
mpifort -c -Wall satisfy_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort satisfy_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm satisfy_mpi.o
mv a.out satisfy
#
mpirun -np 4 ./satisfy > satisfy.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm satisfy
#
echo "Normal end of execution."

