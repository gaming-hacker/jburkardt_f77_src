#! /bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c quickplot_surface.f
if [ $? -ne 0 ]; then
  echo "Errors compiling quickplot_surface.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 quickplot_surface
if [ $? -ne 0 ]; then
  echo "Errors linking and loading quickplot_surface.o."
  exit
fi
#
./quickplot_surface > quickplot_surface_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running quickplot_surface."
  exit
fi
rm quickplot_surface
#
echo "Program output written to quickplot_surface_output.txt"
