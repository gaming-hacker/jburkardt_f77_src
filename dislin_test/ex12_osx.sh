#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 ex12.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex12.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex12
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex12.o"
  exit
fi
rm ex12.o
#
./ex12 > ex12_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex12"
  exit
fi
rm ex12
#
echo "Program output written to ex12_osx_output.txt"
