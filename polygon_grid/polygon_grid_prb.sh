#!/bin/bash
#
gfortran -c polygon_grid_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling polygon_grid_prb.f"
  exit
fi
#
gfortran -o polygon_grid_prb polygon_grid_prb.o -L$HOME/libf77 -lpolygon_grid
if [ $? -ne 0 ]; then
  echo "Errors linking and loading polygon_grid_prb.o"
  exit
fi
rm polygon_grid_prb.o
#
./polygon_grid_prb > polygon_grid_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running polygon_grid_prb"
  exit
fi
rm polygon_grid_prb
#
echo "Test results written to polygon_grid_prb_output.txt."
