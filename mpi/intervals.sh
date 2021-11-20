#! /bin/bash
#
mpifort -c -Wall intervals_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort intervals_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm intervals_mpi.o
mv a.out intervals
#
mpirun -v -np 4 ./intervals > intervals.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm intervals
#
echo "Normal end of execution."
