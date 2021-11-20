#!/bin/bash
#
gfortran -c cube_grid_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling cube_grid_prb.f"
  exit
fi
#
gfortran -o cube_grid_prb cube_grid_prb.o -L$HOME/libf77 -lcube_grid
if [ $? -ne 0 ]; then
  echo "Errors linking and loading cube_grid_prb.o"
  exit
fi
rm cube_grid_prb.o
#
./cube_grid_prb > cube_grid_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running cube_grid_prb"
  exit
fi
rm cube_grid_prb
#
echo "Test program output written to cube_grid_prb_output.txt."
