#!/bin/bash
#
gfortran -c pyramid_monte_carlo_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling pyramid_monte_carlo_prb.f"
  exit
fi
#
gfortran pyramid_monte_carlo_prb.o -L$HOME/libf77 -lpyramid_monte_carlo
if [ $? -ne 0 ]; then
  echo "Errors linking and loading pyramid_monte_carlo_prb.o"
  exit
fi
rm pyramid_monte_carlo_prb.o
#
mv a.out pyramid_monte_carlo_prb
./pyramid_monte_carlo_prb > pyramid_monte_carlo_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running pyramid_monte_carlo_prb"
  exit
fi
rm pyramid_monte_carlo_prb
#
echo "Test program output written to pyramid_monte_carlo_prb_output.txt."
