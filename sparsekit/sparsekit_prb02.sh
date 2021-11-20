#!/bin/bash
#
gfortran -c sparsekit_prb02.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sparsekit_prb02.f"
  exit
fi
#
gfortran sparsekit_prb02.o -L$HOME/libf77 -lsparsekit
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sparsekit_prb02.o"
  exit
fi
rm sparsekit_prb02.o
#
mv a.out sparsekit_prb02
./sparsekit_prb02 > sparsekit_prb02_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sparsekit_prb02"
  exit
fi
rm sparsekit_prb02
#
echo "Program output written to sparsekit_prb02_output.txt"
