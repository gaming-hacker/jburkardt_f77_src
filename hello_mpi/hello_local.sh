#! /bin/bash
#
mpifort -c -Wall hello_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort hello_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hello_mpi.o
mv a.out hello
#
mpirun -np 8 ./hello > hello.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hello
#
echo "Normal end of execution."

