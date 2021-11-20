#!/bin/bash
#
gfortran -c square_grid_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling square_grid_prb.f"
  exit
fi
#
gfortran -o square_grid_prb square_grid_prb.o -L$HOME/libf77 -lsquare_grid
if [ $? -ne 0 ]; then
  echo "Errors linking and loading square_grid_prb.o"
  exit
fi
rm square_grid_prb.o
#
./square_grid_prb > square_grid_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running square_grid_prb"
  exit
fi
rm square_grid_prb
#
echo "Test program output written to square_grid_prb_output.txt."
