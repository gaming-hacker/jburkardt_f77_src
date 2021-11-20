#! /bin/bash
#
mpifort -c -Wall bones_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort bones_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm bones_mpi.o
mv a.out bones
#
mpirun -v -np 4 ./bones > bones.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm bones
#
echo "Normal end of execution."
