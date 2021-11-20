#!/bin/bash
#
gfortran -c hyperball_monte_carlo_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hyperball_monte_carlo_prb.f"
  exit
fi
#
gfortran hyperball_monte_carlo_prb.o -L$HOME/libf77 -lhyperball_monte_carlo
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hyperball_monte_carlo_prb.o"
  exit
fi
rm hyperball_monte_carlo_prb.o
#
mv a.out hyperball_monte_carlo_prb
./hyperball_monte_carlo_prb > hyperball_monte_carlo_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running hyperball_monte_carlo_prb"
  exit
fi
rm hyperball_monte_carlo_prb
#
echo "Test program output written to hyperball_monte_carlo_prb_output.txt."
