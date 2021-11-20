#!/bin/bash
#
gfortran -c simplex_grid_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling simplex_grid_prb.f"
  exit
fi
#
gfortran -o simplex_grid_prb simplex_grid_prb.o -L$HOME/libf77 -lsimplex_grid
if [ $? -ne 0 ]; then
  echo "Errors linking and loading simplex_grid_prb.o"
  exit
fi
rm simplex_grid_prb.o
#
./simplex_grid_prb > simplex_grid_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running simplex_grid_prb"
  exit
fi
rm simplex_grid_prb
#
echo "Test program output written to simplex_grid_prb_output.txt."
