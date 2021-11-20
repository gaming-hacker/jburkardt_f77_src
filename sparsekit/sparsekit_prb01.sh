#!/bin/bash
#
gfortran -c sparsekit_prb01.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sparsekit_prb01.f"
  exit
fi
#
gfortran sparsekit_prb01.o -L$HOME/libf77 -lsparsekit
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sparsekit_prb01.o"
  exit
fi
rm sparsekit_prb01.o
#
mv a.out sparsekit_prb01
./sparsekit_prb01 > sparsekit_prb01_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sparsekit_prb01"
  exit
fi
rm sparsekit_prb01
#
echo "Program output written to sparsekit_prb01_output.txt"
