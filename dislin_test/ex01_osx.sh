#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -g -I$DISLIN/gf/real64 ex01.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex01.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex01
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex01.o"
  exit
fi
rm ex01.o
#
./ex01 > ex01_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex01"
  exit
fi
rm ex01
#
echo "Program output written to ex01_osx_output.txt"
