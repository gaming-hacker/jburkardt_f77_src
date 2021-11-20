#! /bin/bash
#
mpifort -c -Wall matvec_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort matvec_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm matvec_mpi.o
mv a.out matvec
#
mpirun -v -np 4 ./matvec > matvec.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm matvec
#
echo "Normal end of execution."
