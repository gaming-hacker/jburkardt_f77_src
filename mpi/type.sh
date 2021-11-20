#! /bin/bash
#
mpifort -c -Wall type_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort type_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm type_mpi.o
mv a.out type
#
mpirun -v -np 4 ./type > type.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm type
#
echo "Normal end of execution."
