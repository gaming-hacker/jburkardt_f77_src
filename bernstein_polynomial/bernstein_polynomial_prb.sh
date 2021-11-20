#!/bin/bash
#
gfortran -c bernstein_polynomial_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling bernstein_polynomial_prb.f"
  exit
fi
#
gfortran bernstein_polynomial_prb.o -L$HOME/libf77 -lbernstein_polynomial
if [ $? -ne 0 ]; then
  echo "Errors linking and loading bernstein_polynomial_prb.o"
  exit
fi
rm bernstein_polynomial_prb.o
#
mv a.out bernstein_polynomial_prb
./bernstein_polynomial_prb > bernstein_polynomial_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running bernstein_polynomial_prb"
  exit
fi
rm bernstein_polynomial_prb
#
echo "Program output written to bernstein_polynomial_prb_output.txt"
