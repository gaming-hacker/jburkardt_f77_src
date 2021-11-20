#! /bin/bash
#
mpifort -c -Wall day1_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort day1_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm day1_mpi.o
mv a.out day1
#
mpirun -v -np 4 ./day1 > day1.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm day1
#
echo "Normal end of execution."
