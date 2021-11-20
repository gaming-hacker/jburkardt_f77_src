#! /bin/bash
#
mpifort -c -Wall quad2_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort quad2_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm quad2_mpi.o
mv a.out quad2
#
mpirun -np 8 ./quad2 cc_d2_level5 > quad2.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm quad2
#
echo "Normal end of execution."

