#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 ex07.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex07.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex07
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex07.o"
  exit
fi
rm ex07.o
#
./ex07 > ex07_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex07"
  exit
fi
rm ex07
#
echo "Program output written to ex07_osx_output.txt"
