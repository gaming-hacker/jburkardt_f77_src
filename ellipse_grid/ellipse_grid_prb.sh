#!/bin/bash
#
gfortran -c ellipse_grid_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ellipse_grid_prb.f"
  exit
fi
#
gfortran ellipse_grid_prb.o -L$HOME/libf77 -lellipse_grid
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ellipse_grid_prb.o"
  exit
fi
rm ellipse_grid_prb.o
#
mv a.out ellipse_grid_prb
./ellipse_grid_prb > ellipse_grid_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ellipse_grid_prb"
  exit
fi
rm ellipse_grid_prb
#
echo "Test program output written to ellipse_grid_prb_output.txt."
