#!/bin/bash
#
gfortran -c square_monte_carlo_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling square_monte_carlo_prb.f"
  exit
fi
#
gfortran square_monte_carlo_prb.o -L$HOME/libf77 -lsquare_monte_carlo
if [ $? -ne 0 ]; then
  echo "Errors linking and loading square_monte_carlo_prb.o"
  exit
fi
rm square_monte_carlo_prb.o
#
mv a.out square_monte_carlo_prb
./square_monte_carlo_prb > square_monte_carlo_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running square_monte_carlo_prb"
  exit
fi
rm square_monte_carlo_prb
#
echo "Test program output written to square_monte_carlo_prb_output.txt."
