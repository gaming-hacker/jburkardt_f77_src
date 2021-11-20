#!/bin/bash
#
gfortran -c sparsekit_prb13.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sparsekit_prb13.f"
  exit
fi
#
gfortran sparsekit_prb13.o -L$HOME/libf77 -lsparsekit
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sparsekit_prb13.o"
  exit
fi
rm sparsekit_prb13.o
#
mv a.out sparsekit_prb13
./sparsekit_prb13 > sparsekit_prb13_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sparsekit_prb13"
  exit
fi
rm sparsekit_prb13
#
echo "Program output written to sparsekit_prb13_output.txt"
