#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c ex07.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex07.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex07
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex07.o."
  exit
fi
#
./ex07 > ex07_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex07."
  exit
fi
rm ex07
#
echo "Program output written to ex07_output.txt"
