#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c ex04.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex04.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex04
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex04.o."
  exit
fi
#
./ex04 > ex04_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex04."
  exit
fi
rm ex04
#
echo "Program output written to ex04_output.txt"
