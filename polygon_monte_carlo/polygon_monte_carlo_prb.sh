#!/bin/bash
#
gfortran -c polygon_monte_carlo_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling polygon_monte_carlo_prb.f"
  exit
fi
#
gfortran polygon_monte_carlo_prb.o -L$HOME/libf77 -lpolygon_monte_carlo -lpolygon_triangulate
if [ $? -ne 0 ]; then
  echo "Errors linking and loading polygon_monte_carlo_prb.o"
  exit
fi
rm polygon_monte_carlo_prb.o
#
mv a.out polygon_monte_carlo_prb
./polygon_monte_carlo_prb > polygon_monte_carlo_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running polygon_monte_carlo_prb"
  exit
fi
rm polygon_monte_carlo_prb
#
echo "Test program output written to polygon_monte_carlo_prb_output.txt."
