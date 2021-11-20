#!/bin/bash
#
gfortran -c sparsekit_prb05.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sparsekit_prb05.f"
  exit
fi
#
gfortran sparsekit_prb05.o -L$HOME/libf77 -lsparsekit
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sparsekit_prb05.o"
  exit
fi
rm sparsekit_prb05.o
#
mv a.out sparsekit_prb05
./sparsekit_prb05 > sparsekit_prb05_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sparsekit_prb05"
  exit
fi
rm sparsekit_prb05
#
echo "Program output written to sparsekit_prb05_output.txt"
