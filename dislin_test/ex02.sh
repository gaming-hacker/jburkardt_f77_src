#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c ex02.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex02.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex02
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex02.o."
  exit
fi
#
./ex02 > ex02_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex02."
  exit
fi
rm ex02
#
echo "Program output written to ex02_output.txt"
