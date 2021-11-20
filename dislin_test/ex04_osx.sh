#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 ex04.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex04.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex04
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex04.o"
  exit
fi
rm ex04.o
#
./ex04 > ex04_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex04"
  exit
fi
rm ex04
#
echo "Program output written to ex04_osx_output.txt"
