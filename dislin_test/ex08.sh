#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c ex08.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex08.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex08
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex08.o."
  exit
fi
#
./ex08 > ex08_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex08."
  exit
fi
rm ex08
#
echo "Program output written to ex08_output.txt"
