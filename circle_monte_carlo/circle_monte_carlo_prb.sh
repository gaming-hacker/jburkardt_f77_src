#!/bin/bash
#
gfortran -c circle_monte_carlo_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling circle_monte_carlo_prb.f"
  exit
fi
#
gfortran circle_monte_carlo_prb.o -L$HOME/libf77 -lcircle_monte_carlo
if [ $? -ne 0 ]; then
  echo "Errors linking and loading circle_monte_carlo_prb.o"
  exit
fi
rm circle_monte_carlo_prb.o
#
mv a.out circle_monte_carlo_prb
./circle_monte_carlo_prb > circle_monte_carlo_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running circle_monte_carlo_prb"
  exit
fi
rm circle_monte_carlo_prb
#
echo "Test program output written to circle_monte_carlo_prb_output.txt."
