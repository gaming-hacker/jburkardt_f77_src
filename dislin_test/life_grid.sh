#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c life_grid.f
if [ $? -ne 0 ]; then
  echo "Errors compiling life_grid.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 life_grid
if [ $? -ne 0 ]; then
  echo "Errors linking and loading life_grid.o."
  exit
fi
#
./life_grid > life_grid_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running life_grid."
  exit
fi
rm life_grid
#
echo "Program output written to life_grid_output.txt"
