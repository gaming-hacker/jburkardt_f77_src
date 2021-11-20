#!/bin/bash
#
gfortran -c hypersphere_monte_carlo_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hypersphere_monte_carlo_prb.f"
  exit
fi
#
gfortran hypersphere_monte_carlo_prb.o -L$HOME/libf77 -lhypersphere_monte_carlo
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hypersphere_monte_carlo_prb.o"
  exit
fi
rm hypersphere_monte_carlo_prb.o
#
mv a.out hypersphere_monte_carlo_prb
./hypersphere_monte_carlo_prb > hypersphere_monte_carlo_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running hypersphere_monte_carlo_prb"
  exit
fi
rm hypersphere_monte_carlo_prb
#
echo "Test program output written to hypersphere_monte_carlo_prb_output.txt."
