#!/bin/bash
#
gfortran -c monomial_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling monomial_prb.f"
  exit
fi
#
gfortran monomial_prb.o -L$HOME/libf77 -lmonomial
if [ $? -ne 0 ]; then
  echo "Errors linking and loading monomial_prb.o"
  exit
fi
rm monomial_prb.o
#
mv a.out monomial_prb
./monomial_prb > monomial_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running monomial_prb"
  exit
fi
rm monomial_prb
#
echo "Test program output written to monomial_prb_output.txt."
