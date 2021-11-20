#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c ex06.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex06.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex06
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex06.o."
  exit
fi
#
./ex06 > ex06_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex06."
  exit
fi
rm ex06
#
echo "Program output written to ex06_output.txt"
