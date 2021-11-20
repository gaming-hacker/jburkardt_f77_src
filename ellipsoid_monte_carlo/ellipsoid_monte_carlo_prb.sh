#!/bin/bash
#
gfortran -c ellipsoid_monte_carlo_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ellipsoid_monte_carlo_prb.f"
  exit
fi
#
gfortran -o ellipsoid_monte_carlo_prb ellipsoid_monte_carlo_prb.o -L$HOME/libf77 -lellipsoid_monte_carlo
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ellipsoid_monte_carlo_prb.o"
  exit
fi
rm ellipsoid_monte_carlo_prb.o
#
./ellipsoid_monte_carlo_prb > ellipsoid_monte_carlo_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ellipsoid_monte_carlo_prb"
  exit
fi
rm ellipsoid_monte_carlo_prb
#
echo "Test results written to ellipsoid_monte_carlo_prb_output.txt."
