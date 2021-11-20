#!/bin/bash
#
gfortran -c prob_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling prob_prb.f"
  exit
fi
#
gfortran prob_prb.o -L$HOME/libf77 -lprob
if [ $? -ne 0 ]; then
  echo "Errors linking and loading prob_prb.o"
  exit
fi
rm prob_prb.o
#
mv a.out prob_prb
./prob_prb > prob_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running prob_prb"
  exit
fi
rm prob_prb
#
echo "Test program output written to prob_prb_output.txt."
