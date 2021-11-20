#! /bin/bash
#
mpifort -c -Wall heat_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort heat_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm heat_mpi.o
mv a.out heat
#
mpirun -np 8 ./heat > heat.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm heat
#
echo "Normal end of execution."

