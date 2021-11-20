#!/bin/bash
#
gfortran -c sparsekit_prb09.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sparsekit_prb09.f"
  exit
fi
#
gfortran sparsekit_prb09.o -L$HOME/libf77 -lsparsekit
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sparsekit_prb09.o"
  exit
fi
rm sparsekit_prb09.o
#
mv a.out sparsekit_prb09
./sparsekit_prb09 > sparsekit_prb09_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sparsekit_prb09"
  exit
fi
rm sparsekit_prb09
#
echo "Program output written to sparsekit_prb09_output.txt"
