#!/bin/bash
#
gfortran -c simplex_integrals_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling simplex_integrals_prb.f"
  exit
fi
#
gfortran simplex_integrals_prb.o -L$HOME/libf77 -lsimplex_integrals
if [ $? -ne 0 ]; then
  echo "Errors linking and loading simplex_integrals_prb.o"
  exit
fi
rm simplex_integrals_prb.o
#
mv a.out simplex_integrals_prb
./simplex_integrals_prb > simplex_integrals_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running simplex_integrals_prb"
  exit
fi
rm simplex_integrals_prb
#
echo "Test program output written to simplex_integrals_prb_output.txt."
