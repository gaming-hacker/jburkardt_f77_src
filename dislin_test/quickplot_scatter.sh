#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c quickplot_scatter.f
if [ $? -ne 0 ]; then
  echo "Errors compiling quickplot_scatter.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 quickplot_scatter
if [ $? -ne 0 ]; then
  echo "Errors linking and loading quickplot_scatter.o."
  exit
fi
#
./quickplot_scatter > quickplot_scatter_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running quickplot_scatter."
  exit
fi
rm quickplot_scatter
#
echo "Program output written to quickplot_scatter_output.txt"
