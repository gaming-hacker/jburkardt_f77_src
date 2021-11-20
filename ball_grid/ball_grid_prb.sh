#!/bin/bash
#
gfortran -c ball_grid_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ball_grid_prb.f"
  exit
fi
#
gfortran ball_grid_prb.o -L$HOME/libf77 -lball_grid
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ball_grid_prb.o"
  exit
fi
rm ball_grid_prb.o
#
mv a.out ball_grid_prb
./ball_grid_prb > ball_grid_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ball_grid_prb"
  exit
fi
rm ball_grid_prb
#
echo "Test program output written to ball_grid_prb_output.txt."
