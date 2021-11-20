#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 ex07b.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex07b.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex07b
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex07b.o"
  exit
fi
rm ex07b.o
#
./ex07b > ex07b_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex07b"
  exit
fi
rm ex07b
#
echo "Program output written to ex07b_osx_output.txt"
