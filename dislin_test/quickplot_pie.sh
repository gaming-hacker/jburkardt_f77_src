#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c quickplot_pie.f
if [ $? -ne 0 ]; then
  echo "Errors compiling quickplot_pie.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 quickplot_pie
if [ $? -ne 0 ]; then
  echo "Errors linking and loading quickplot_pie.o."
  exit
fi
#
./quickplot_pie > quickplot_pie_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running quickplot_pie."
  exit
fi
rm quickplot_pie
#
echo "Program output written to quickplot_pie_output.txt"
