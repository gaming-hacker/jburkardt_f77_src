#!/bin/bash
#
gfortran -c ellipsoid_grid_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ellipsoid_grid_prb.f"
  exit
fi
#
gfortran ellipsoid_grid_prb.o -L$HOME/libf77 -lellipsoid_grid
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ellipsoid_grid_prb.o"
  exit
fi
rm ellipsoid_grid_prb.o
#
mv a.out ellipsoid_grid_prb
./ellipsoid_grid_prb > ellipsoid_grid_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ellipsoid_grid_prb"
  exit
fi
rm ellipsoid_grid_prb
#
echo "Test program output written to ellipsoid_grid_prb_output.txt."
