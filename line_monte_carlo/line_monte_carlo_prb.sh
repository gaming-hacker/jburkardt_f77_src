#!/bin/bash
#
gfortran -c line_monte_carlo_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling line_monte_carlo_prb.f"
  exit
fi
#
gfortran line_monte_carlo_prb.o -L$HOME/libf77 -lline_monte_carlo
if [ $? -ne 0 ]; then
  echo "Errors linking and loading line_monte_carlo_prb.o"
  exit
fi
rm line_monte_carlo_prb.o
#
mv a.out line_monte_carlo_prb
./line_monte_carlo_prb > line_monte_carlo_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running line_monte_carlo_prb"
  exit
fi
rm line_monte_carlo_prb
#
echo "Test results written to line_monte_carlo_prb_output.txt."
