#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 ex13.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex13.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex13
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex13.o"
  exit
fi
rm ex13.o
#
./ex13 > ex13_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex13"
  exit
fi
rm ex13
#
echo "Program output written to ex13_osx_output.txt"
