#!/bin/bash
#
gfortran -c monomial_value_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling monomial_value_prb.f"
  exit
fi
#
gfortran -o monomial_value_prb monomial_value_prb.o -L$HOME/libf77 -lmonomial_value
if [ $? -ne 0 ]; then
  echo "Errors linking and loading monomial_value_prb.o"
  exit
fi
rm monomial_value_prb.o
#
./monomial_value_prb > monomial_value_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running monomial_value_prb"
  exit
fi
rm monomial_value_prb
#
echo "Test program output written to monomial_value_prb_output.txt."
