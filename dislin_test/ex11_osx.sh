#!/bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my OSX system.
#
export DISLIN=/usr/local/dislin
export DYLD_LIBRARY_PATH=$DISLIN:$DYLD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 ex11.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ex11.f"
  exit
fi
#
$DISLIN/bin/gf77link -r8 ex11
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ex11.o"
  exit
fi
rm ex11.o
#
./ex11 > ex11_osx_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ex11"
  exit
fi
rm ex11
#
echo "Program output written to ex11_osx_output.txt"
