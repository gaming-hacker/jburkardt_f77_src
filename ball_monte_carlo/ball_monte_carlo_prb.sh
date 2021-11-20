#!/bin/bash
#
gfortran -c ball_monte_carlo_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ball_monte_carlo_prb.f"
  exit
fi
#
gfortran ball_monte_carlo_prb.o -L$HOME/libf77 -lball_monte_carlo
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ball_monte_carlo_prb.o"
  exit
fi
rm ball_monte_carlo_prb.o
#
mv a.out ball_monte_carlo_prb
./ball_monte_carlo_prb > ball_monte_carlo_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ball_monte_carlo_prb"
  exit
fi
rm ball_monte_carlo_prb
#
echo "Test program output written to ball_monte_carlo_prb_output.txt."
