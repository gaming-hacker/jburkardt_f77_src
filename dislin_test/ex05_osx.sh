#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 ex05.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex05.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex05
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex05.o"
  exit
fi
rm ex05.o
#
./ex05 > ex05_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex05"
  exit
fi
rm ex05
#
echo "Program output written to ex05_osx_output.txt"
