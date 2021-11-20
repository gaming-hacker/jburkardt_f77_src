#! /bin/bash
#
mpifort -c -Wall quadrature_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort quadrature_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm quadrature_mpi.o
mv a.out quadrature
#
mpirun -v -np 4 ./quadrature > quadrature.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm quadrature
#
echo "Normal end of execution."
