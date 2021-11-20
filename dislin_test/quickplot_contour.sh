#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c quickplot_contour.f
if [ $? -ne 0 ]; then
  echo "Errors compiling quickplot_contour.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 quickplot_contour
if [ $? -ne 0 ]; then
  echo "Errors linking and loading quickplot_contour.o."
  exit
fi
#
./quickplot_contour > quickplot_contour_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running quickplot_contour."
  exit
fi
rm quickplot_contour
#
echo "Program output written to quickplot_contour_output.txt"
