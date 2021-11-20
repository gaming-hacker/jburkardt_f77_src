#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 life_grid.f
if [ $? -ne 0 ]; then
  echo "Errors compiling life_grid.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 life_grid
if [ $? -ne 0 ]; then
  echo "Errors linking and loading life_grid.o"
  exit
fi
rm life_grid.o
#
./life_grid > life_grid_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running life_grid"
  exit
fi
rm life_grid
#
echo "Program output written to life_grid_osx_output.txt"
