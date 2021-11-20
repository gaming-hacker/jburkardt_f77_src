#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c ex10.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex10.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex10
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex10.o."
  exit
fi
#
./ex10 > ex10_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex10."
  exit
fi
rm ex10
#
echo "Program output written to ex10_output.txt"
