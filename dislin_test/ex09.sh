#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c ex09.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex09.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex09
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex09.o."
  exit
fi
#
./ex09 > ex09_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex09."
  exit
fi
rm ex09
#
echo "Program output written to ex09_output.txt"
