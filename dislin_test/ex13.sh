#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c ex13.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex13.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex13
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex13.o."
  exit
fi
#
./ex13 > ex13_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex13."
  exit
fi
rm ex13
#
echo "Program output written to ex13_output.txt"
