#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c ex12.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex12.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex12
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex12.o."
  exit
fi
#
./ex12 > ex12_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex12."
  exit
fi
rm ex12
#
echo "Program output written to ex12_output.txt"
