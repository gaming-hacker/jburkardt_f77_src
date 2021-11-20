#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c ex07b.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex07b.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex07b
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex07b.o."
  exit
fi
#
./ex07b > ex07b_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex07b."
  exit
fi
rm ex07b
#
echo "Program output written to ex07b_output.txt"
