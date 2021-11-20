#!/bin/bash
#
gfortran -c polynomial_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling polynomial_prb.f"
  exit
fi
#
gfortran polynomial_prb.o -L$HOME/libf77 -lpolynomial
if [ $? -ne 0 ]; then
  echo "Errors linking and loading polynomial_prb.o"
  exit
fi
rm polynomial_prb.o
#
mv a.out polynomial_prb
./polynomial_prb > polynomial_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running polynomial_prb"
  exit
fi
rm polynomial_prb
#
echo "Test program output written to polynomial_prb_output.txt."
