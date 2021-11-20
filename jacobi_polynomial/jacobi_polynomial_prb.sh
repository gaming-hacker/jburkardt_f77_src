#!/bin/bash
#
gfortran -c jacobi_polynomial_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling jacobi_polynomial_prb.f"
  exit
fi
#
gfortran jacobi_polynomial_prb.o -L$HOME/libf77 -ljacobi_polynomial
if [ $? -ne 0 ]; then
  echo "Errors linking and loading jacobi_polynomial_prb.o"
  exit
fi
rm jacobi_polynomial_prb.o
#
mv a.out jacobi_polynomial_prb
./jacobi_polynomial_prb > jacobi_polynomial_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running jacobi_polynomial_prb"
  exit
fi
rm jacobi_polynomial_prb
#
echo "Program output written to jacobi_polynomial_prb_output.txt"
