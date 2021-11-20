#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 quickplot_scatter.f
if [ $? -ne 0 ]; then
  echo "Errors compiling quickplot_scatter.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 quickplot_scatter
if [ $? -ne 0 ]; then
  echo "Errors linking and loading quickplot_scatter.o"
  exit
fi
rm quickplot_scatter.o
#
./quickplot_scatter > quickplot_scatter_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running quickplot_scatter"
  exit
fi
rm quickplot_scatter
#
echo "Program output written to quickplot_scatter_osx_output.txt"
