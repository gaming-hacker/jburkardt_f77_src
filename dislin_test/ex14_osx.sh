#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 ex14.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex14.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex14
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex14.o"
  exit
fi
rm ex14.o
#
./ex14 > ex14_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex14"
  exit
fi
rm ex14
#
echo "Program output written to ex14_osx_output.txt"
