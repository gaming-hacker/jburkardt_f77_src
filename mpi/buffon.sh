#! /bin/bash
#
mpifort -c -Wall buffon_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort buffon_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm buffon_mpi.o
mv a.out buffon
#
mpirun -v -np 4 ./buffon > buffon.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm buffon
#
echo "Normal end of execution."
