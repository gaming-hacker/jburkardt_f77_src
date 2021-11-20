#!/bin/bash
#
gfortran -c matrix_exponential_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling matrix_exponential_prb.f"
  exit
fi
#
gfortran matrix_exponential_prb.o -L$HOME/libf77 -lmatrix_exponential \
  -ltest_matrix_exponential -lr8lib -lc8lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading matrix_exponential_prb.o"
  exit
fi
rm matrix_exponential_prb.o
#
mv a.out matrix_exponential_prb
./matrix_exponential_prb > matrix_exponential_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running matrix_exponential_prb"
  exit
fi
rm matrix_exponential_prb
#
echo "Test program output written to matrix_exponential_prb_output.txt."
