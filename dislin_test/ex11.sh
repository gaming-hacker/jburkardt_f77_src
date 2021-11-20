#!/bin/bash
#
#  This script is designed to compile and run F77 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c ex11.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex11.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex11
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex11.o."
  exit
fi
#
./ex11 > ex11_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex11."
  exit
fi
rm ex11
#
echo "Program output written to ex11_output.txt"
