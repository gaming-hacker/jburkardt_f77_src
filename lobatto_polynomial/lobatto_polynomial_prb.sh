#!/bin/bash
#
gfortran -c lobatto_polynomial_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling lobatto_polynomial_prb.f"
  exit
fi
#
gfortran -o lobatto_polynomial_prb lobatto_polynomial_prb.o -L$HOME/libf77 -llobatto_polynomial
if [ $? -ne 0 ]; then
  echo "Errors linking and loading lobatto_polynomial_prb.o"
  exit
fi
rm lobatto_polynomial_prb.o
#
./lobatto_polynomial_prb > lobatto_polynomial_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running lobatto_polynomial_prb"
  exit
fi
rm lobatto_polynomial_prb
#
echo "Test program output written to lobatto_polynomial_prb_output.txt."
