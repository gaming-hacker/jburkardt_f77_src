#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 ex03.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex03.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex03
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex03.o"
  exit
fi
rm ex03.o
#
./ex03 > ex03_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex03"
  exit
fi
rm ex03
#
echo "Program output written to ex03_osx_output.txt"
