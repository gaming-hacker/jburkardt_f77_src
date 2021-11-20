#! /bin/bash
#
gfortran -c ellipse_monte_carlo_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ellipse_monte_carlo_prb.f"
  exit
fi
#
gfortran ellipse_monte_carlo_prb.o -L$HOME/libf77 -lellipse_monte_carlo
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ellipse_monte_carlo_prb.o"
  exit
fi
rm ellipse_monte_carlo_prb.o
#
mv a.out ellipse_monte_carlo_prb
./ellipse_monte_carlo_prb > ellipse_monte_carlo_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ellipse_monte_carlo_prb"
  exit
fi
rm ellipse_monte_carlo_prb
#
echo "Test program output written to ellipse_monte_carlo_prb_output.txt."
