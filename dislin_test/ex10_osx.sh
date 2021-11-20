#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 ex10.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex10.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex10
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex10.o"
  exit
fi
rm ex10.o
#
./ex10 > ex10_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex10"
  exit
fi
rm ex10
#
echo "Program output written to ex10_osx_output.txt"
