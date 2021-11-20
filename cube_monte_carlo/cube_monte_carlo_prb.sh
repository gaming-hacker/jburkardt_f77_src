#!/bin/bash
#
gfortran -c cube_monte_carlo_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling cube_monte_carlo_prb.f"
  exit
fi
#
gfortran cube_monte_carlo_prb.o -L$HOME/libf77 -lcube_monte_carlo
if [ $? -ne 0 ]; then
  echo "Errors linking and loading cube_monte_carlo_prb.o"
  exit
fi
rm cube_monte_carlo_prb.o
#
mv a.out cube_monte_carlo_prb
./cube_monte_carlo_prb > cube_monte_carlo_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running cube_monte_carlo_prb"
  exit
fi
rm cube_monte_carlo_prb
#
echo "Test program output written to cube_monte_carlo_prb_output.txt."
