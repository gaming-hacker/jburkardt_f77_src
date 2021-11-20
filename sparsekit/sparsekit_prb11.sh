#!/bin/bash
#
gfortran -c sparsekit_prb11.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sparsekit_prb11.f"
  exit
fi
#
gfortran sparsekit_prb11.o -L$HOME/libf77 -lsparsekit
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sparsekit_prb11.o"
  exit
fi
rm sparsekit_prb11.o
#
mv a.out sparsekit_prb11
./sparsekit_prb11 > sparsekit_prb11_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sparsekit_prb11"
  exit
fi
rm sparsekit_prb11
#
echo "Program output written to sparsekit_prb11_output.txt"
