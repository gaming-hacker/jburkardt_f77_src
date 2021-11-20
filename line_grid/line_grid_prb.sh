#!/bin/bash
#
gfortran -c line_grid_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling line_grid_prb.f"
  exit
fi
#
gfortran -o line_grid_prb line_grid_prb.o -L$HOME/libf77 -lline_grid
if [ $? -ne 0 ]; then
  echo "Errors linking and loading line_grid_prb.o"
  exit
fi
rm line_grid_prb.o
#
./line_grid_prb > line_grid_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running line_grid_prb"
  exit
fi
rm line_grid_prb
#
echo "Test program output written to line_grid_prb_output.txt."
