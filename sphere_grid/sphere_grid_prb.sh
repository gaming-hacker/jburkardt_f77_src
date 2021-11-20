#!/bin/bash
#
gfortran -c sphere_grid_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sphere_grid_prb.f"
  exit
fi
#
gfortran sphere_grid_prb.o -L$HOME/libf77 -lsphere_grid
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sphere_grid_prb.o"
  exit
fi
rm sphere_grid_prb.o
#
mv a.out sphere_grid_prb
./sphere_grid_prb > sphere_grid_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running sphere_grid_prb"
  exit
fi
rm sphere_grid_prb
#
mv *.xyz ../../datasets/sphere_grid
echo "XYZ data files moved to ../../datasets/sphere_grid directory."
#
echo "Test program output written to sphere_grid_prb_output.txt."
