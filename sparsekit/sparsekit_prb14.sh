#!/bin/bash
#
gfortran -c sparsekit_prb14.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sparsekit_prb14.f"
  exit
fi
#
gfortran sparsekit_prb14.o -L$HOME/libf77 -lsparsekit
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sparsekit_prb14.o"
  exit
fi
rm sparsekit_prb14.o
#
mv a.out sparsekit_prb14
./sparsekit_prb14 > sparsekit_prb14_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sparsekit_prb14"
  exit
fi
rm sparsekit_prb14
#
echo "Program output written to sparsekit_prb14_output.txt"
