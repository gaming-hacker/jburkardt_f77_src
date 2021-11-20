#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c quickplot_bar.f
if [ $? -ne 0 ]; then
  echo "Errors compiling quickplot_bar.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 quickplot_bar
if [ $? -ne 0 ]; then
  echo "Errors linking and loading quickplot_bar.o."
  exit
fi
#
./quickplot_bar > quickplot_bar_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running quickplot_bar."
  exit
fi
rm quickplot_bar
#
echo "Program output written to quickplot_bar_output.txt"
