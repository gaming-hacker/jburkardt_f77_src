#!/bin/bash
#
gfortran -c hypercube_monte_carlo_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hypercube_monte_carlo_prb.f"
  exit
fi
#
gfortran hypercube_monte_carlo_prb.o -L$HOME/libf77 -lhypercube_monte_carlo
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hypercube_monte_carlo_prb.o"
  exit
fi
rm hypercube_monte_carlo_prb.o
#
mv a.out hypercube_monte_carlo_prb
./hypercube_monte_carlo_prb > hypercube_monte_carlo_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running hypercube_monte_carlo_prb"
  exit
fi
rm hypercube_monte_carlo_prb
#
echo "Test program output written to hypercube_monte_carlo_prb_output.txt."
