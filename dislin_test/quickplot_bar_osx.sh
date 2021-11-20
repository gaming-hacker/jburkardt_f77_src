#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 quickplot_bar.f
if [ $? -ne 0 ]; then
  echo "Errors compiling quickplot_bar.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 quickplot_bar
if [ $? -ne 0 ]; then
  echo "Errors linking and loading quickplot_bar.o"
  exit
fi
rm quickplot_bar.o
#
./quickplot_bar > quickplot_bar_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running quickplot_bar"
  exit
fi
rm quickplot_bar
#
echo "Program output written to quickplot_bar_osx_output.txt"
