#! /bin/bash
#
mpifort -c -Wall matmat_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort matmat_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm matmat_mpi.o
mv a.out matmat
#
mpirun -v -np 4 ./matmat > matmat.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm matmat
#
echo "Normal end of execution."
