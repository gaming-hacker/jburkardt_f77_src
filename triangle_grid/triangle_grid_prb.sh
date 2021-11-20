#!/bin/bash
#
gfortran -c triangle_grid_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling triangle_grid_prb.f"
  exit
fi
#
gfortran triangle_grid_prb.o -L$HOME/libf77 -ltriangle_grid
if [ $? -ne 0 ]; then
  echo "Errors linking and loading triangle_grid_prb.o"
  exit
fi
rm triangle_grid_prb.o
#
mv a.out triangle_grid_prb
./triangle_grid_prb > triangle_grid_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running triangle_grid_prb"
  exit
fi
rm triangle_grid_prb
#
echo "Test program output written to triangle_grid_prb_output.txt."
