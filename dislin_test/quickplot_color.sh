#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c quickplot_color.f
if [ $? -ne 0 ]; then
  echo "Errors compiling quickplot_color.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 quickplot_color
if [ $? -ne 0 ]; then
  echo "Errors linking and loading quickplot_color.o."
  exit
fi
#
./quickplot_color > quickplot_color_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running quickplot_color."
  exit
fi
rm quickplot_color
#
echo "Program output written to quickplot_color_output.txt"
