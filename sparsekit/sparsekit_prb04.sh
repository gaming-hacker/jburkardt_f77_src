#!/bin/bash
#
gfortran -c sparsekit_prb04.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sparsekit_prb04.f"
  exit
fi
#
gfortran sparsekit_prb04.o -L$HOME/libf77 -lsparsekit
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sparsekit_prb04.o"
  exit
fi
rm sparsekit_prb04.o
#
mv a.out sparsekit_prb04
./sparsekit_prb04 > sparsekit_prb04_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sparsekit_prb04"
  exit
fi
rm sparsekit_prb04
#
echo "Program output written to sparsekit_prb04_output.txt"
