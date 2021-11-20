#! /bin/bash
#
mpifort -c -Wall multitask_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort multitask_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm multitask_mpi.o
mv a.out multitask
#
mpirun -np 8 ./multitask > multitask.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm multitask
#
echo "Normal end of execution."

