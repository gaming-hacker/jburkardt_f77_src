#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c ex05.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex05.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex05
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex05.o."
  exit
fi
#
./ex05 > ex05_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex05."
  exit
fi
rm ex05
#
echo "Program output written to ex05_output.txt"
