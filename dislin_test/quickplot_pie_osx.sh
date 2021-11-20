#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 quickplot_pie.f
if [ $? -ne 0 ]; then
  echo "Errors compiling quickplot_pie.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 quickplot_pie
if [ $? -ne 0 ]; then
  echo "Errors linking and loading quickplot_pie.o"
  exit
fi
rm quickplot_pie.o
#
./quickplot_pie > quickplot_pie_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running quickplot_pie"
  exit
fi
rm quickplot_pie
#
echo "Program output written to quickplot_pie_osx_output.txt"
