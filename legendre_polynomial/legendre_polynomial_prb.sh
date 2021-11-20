#!/bin/bash
#
gfortran -c legendre_polynomial_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling legendre_polynomial_prb.f"
  exit
fi
#
gfortran legendre_polynomial_prb.o -L$HOME/libf77 -llegendre_polynomial
if [ $? -ne 0 ]; then
  echo "Errors linking and loading legendre_polynomial_prb.o"
  exit
fi
rm legendre_polynomial_prb.o
#
mv a.out legendre_polynomial_prb
./legendre_polynomial_prb > legendre_polynomial_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running legendre_polynomial_prb"
  exit
fi
rm legendre_polynomial_prb
#
echo "Test program output written to legendre_polynomial_prb_output.txt."
