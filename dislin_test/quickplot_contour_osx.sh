#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 quickplot_contour.f
if [ $? -ne 0 ]; then
  echo "Errors compiling quickplot_contour.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 quickplot_contour
if [ $? -ne 0 ]; then
  echo "Errors linking and loading quickplot_contour.o"
  exit
fi
rm quickplot_contour.o
#
./quickplot_contour > quickplot_contour_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running quickplot_contour"
  exit
fi
rm quickplot_contour
#
echo "Program output written to quickplot_contour_osx_output.txt"
