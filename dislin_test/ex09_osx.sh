#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 ex09.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex09.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex09
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex09.o"
  exit
fi
rm ex09.o
#
./ex09 > ex09_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex09"
  exit
fi
rm ex09
#
echo "Program output written to ex09_osx_output.txt"
