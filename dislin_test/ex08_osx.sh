#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 ex08.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex08.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex08
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex08.o"
  exit
fi
rm ex08.o
#
./ex08 > ex08_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex08"
  exit
fi
rm ex08
#
echo "Program output written to ex08_osx_output.txt"
