#!/bin/bash
#
gfortran -c sparsekit_prb12.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sparsekit_prb12.f"
  exit
fi
#
gfortran sparsekit_prb12.o -L$HOME/libf77 -lsparsekit
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sparsekit_prb12.o"
  exit
fi
rm sparsekit_prb12.o
#
mv a.out sparsekit_prb12
./sparsekit_prb12 > sparsekit_prb12_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sparsekit_prb12"
  exit
fi
rm sparsekit_prb12
#
echo "Program output written to sparsekit_prb12_output.txt"
