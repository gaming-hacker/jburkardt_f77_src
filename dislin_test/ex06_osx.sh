#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 ex06.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex06.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex06
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex06.o"
  exit
fi
rm ex06.o
#
./ex06 > ex06_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex06"
  exit
fi
rm ex06
#
echo "Program output written to ex06_osx_output.txt"
