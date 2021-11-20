#!/bin/bash
#
gfortran -c simplex_monte_carlo_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling simplex_monte_carlo_prb.f"
  exit
fi
#
gfortran simplex_monte_carlo_prb.o -L$HOME/libf77 -lsimplex_monte_carlo
if [ $? -ne 0 ]; then
  echo "Errors linking and loading simplex_monte_carlo_prb.o"
  exit
fi
rm simplex_monte_carlo_prb.o
#
mv a.out simplex_monte_carlo_prb
./simplex_monte_carlo_prb > simplex_monte_carlo_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running simplex_monte_carlo_prb"
  exit
fi
rm simplex_monte_carlo_prb
#
echo "Test program output written to simplex_monte_carlo_prb_output.txt."
