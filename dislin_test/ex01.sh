#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c -g ex01.f >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling ex01.f"
  exit
fi
rm compiler.txt
#
$DISLIN/bin/gf77link -r8 ex01
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex01.o."
  exit
fi
#
./ex01 > ex01_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex01."
  exit
fi
rm ex01
#
echo "Program output written to ex01_output.txt"
