#! /bin/bash
#
mpifort -c -Wall search_mpi.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort search_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm search_mpi.o
mv a.out search
#
mpirun -v -np 4 ./search > search.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm search
#
echo "Normal end of execution."
