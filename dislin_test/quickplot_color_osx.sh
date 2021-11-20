#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 quickplot_color.f
if [ $? -ne 0 ]; then
  echo "Errors compiling quickplot_color.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 quickplot_color
if [ $? -ne 0 ]; then
  echo "Errors linking and loading quickplot_color.o"
  exit
fi
rm quickplot_color.o
#
./quickplot_color > quickplot_color_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running quickplot_color"
  exit
fi
rm quickplot_color
#
echo "Program output written to quickplot_color_osx_output.txt"
