#!/bin/bash
#
gfortran -c hermite_polynomial_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hermite_polynomial_prb.f"
  exit
fi
#
gfortran hermite_polynomial_prb.o -L$HOME/libf77 -lhermite_polynomial
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hermite_polynomial_prb.o"
  exit
fi
rm hermite_polynomial_prb.o
#
mv a.out hermite_polynomial_prb
./hermite_polynomial_prb > hermite_polynomial_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running hermite_polynomial_prb"
  exit
fi
rm hermite_polynomial_prb
#
echo "Test program output written to hermite_polynomial_prb_output.txt."
