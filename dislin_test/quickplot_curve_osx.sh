#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 quickplot_curve.f
if [ $? -ne 0 ]; then
  echo "Errors compiling quickplot_curve.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 quickplot_curve
if [ $? -ne 0 ]; then
  echo "Errors linking and loading quickplot_curve.o"
  exit
fi
rm quickplot_curve.o
#
./quickplot_curve > quickplot_curve_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running quickplot_curve"
  exit
fi
rm quickplot_curve
#
echo "Program output written to quickplot_curve_osx_output.txt"
