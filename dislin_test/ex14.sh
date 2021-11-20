#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c ex14.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex14.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex14
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex14.o."
  exit
fi
#
./ex14 > ex14_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex14."
  exit
fi
rm ex14
#
echo "Program output written to ex14_output.txt"
