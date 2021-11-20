#!/bin/bash
#
gfortran -c sparsekit_prb08.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sparsekit_prb08.f"
  exit
fi
#
gfortran sparsekit_prb08.o -L$HOME/libf77 -lsparsekit
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sparsekit_prb08.o"
  exit
fi
rm sparsekit_prb08.o
#
mv a.out sparsekit_prb08
./sparsekit_prb08 > sparsekit_prb08_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sparsekit_prb08"
  exit
fi
rm sparsekit_prb08
#
echo "Program output written to sparsekit_prb08_output.txt"
