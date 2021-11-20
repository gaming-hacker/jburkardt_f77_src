#!/bin/bash
#
gfortran -c sparsekit_prb06.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sparsekit_prb06.f"
  exit
fi
#
gfortran sparsekit_prb06.o -L$HOME/libf77 -lsparsekit
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sparsekit_prb06.o"
  exit
fi
rm sparsekit_prb06.o
#
mv a.out sparsekit_prb06
./sparsekit_prb06 > sparsekit_prb06_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sparsekit_prb06"
  exit
fi
rm sparsekit_prb06
#
echo "Program output written to sparsekit_prb06_output.txt"
