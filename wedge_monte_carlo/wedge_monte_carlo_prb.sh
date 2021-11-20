#!/bin/bash
#
gfortran -c wedge_monte_carlo_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling wedge_monte_carlo_prb.f"
  exit
fi
#
gfortran wedge_monte_carlo_prb.o -L$HOME/libf77 -lwedge_monte_carlo
if [ $? -ne 0 ]; then
  echo "Errors linking and loading wedge_monte_carlo_prb.o"
  exit
fi
rm wedge_monte_carlo_prb.o
#
mv a.out wedge_monte_carlo_prb
./wedge_monte_carlo_prb > wedge_monte_carlo_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running wedge_monte_carlo_prb"
  exit
fi
rm wedge_monte_carlo_prb
#
echo "Test program output written to wedge_monte_carlo_prb_output.txt."
