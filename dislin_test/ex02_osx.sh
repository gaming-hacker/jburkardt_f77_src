#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -g -I$DISLIN/gf/real64 ex02.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex02.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex02
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex02.o"
  exit
fi
rm ex02.o
#
./ex02 > ex02_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex02"
  exit
fi
rm ex02
#
echo "Program output written to ex02_osx_output.txt"
