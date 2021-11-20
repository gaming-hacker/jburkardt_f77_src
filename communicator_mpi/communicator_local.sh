#! /bin/bash
#
mpifort -c -Wall communicator_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort communicator_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm communicator_mpi.o
mv a.out communicator
#
mpirun -np 4 ./communicator > communicator.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm communicator
#
echo "Normal end of execution."
