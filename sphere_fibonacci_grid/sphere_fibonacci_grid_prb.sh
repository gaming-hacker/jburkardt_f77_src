#!/bin/bash
#
gfortran -c sphere_fibonacci_grid_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sphere_fibonacci_grid_prb.f"
  exit
fi
#
gfortran -o sphere_fibonacci_grid_prb sphere_fibonacci_grid_prb.o -L$HOME/libf77 -lsphere_fibonacci_grid
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sphere_fibonacci_grid_prb.o"
  exit
fi
rm sphere_fibonacci_grid_prb.o
#
./sphere_fibonacci_grid_prb > sphere_fibonacci_grid_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sphere_fibonacci_grid_prb"
  exit
fi
rm sphere_fibonacci_grid_prb
#
echo "Test program output written to sphere_fibonacci_grid_prb_output.txt."
