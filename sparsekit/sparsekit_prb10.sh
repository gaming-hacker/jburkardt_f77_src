#!/bin/bash
#
gfortran -c sparsekit_prb10.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sparsekit_prb10.f"
  exit
fi
#
gfortran sparsekit_prb10.o -L$HOME/libf77 -lsparsekit
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sparsekit_prb10.o"
  exit
fi
rm sparsekit_prb10.o
#
mv a.out sparsekit_prb10
./sparsekit_prb10 > sparsekit_prb10_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sparsekit_prb10"
  exit
fi
rm sparsekit_prb10
#
echo "Program output written to sparsekit_prb10_output.txt"
