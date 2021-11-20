#!/bin/bash
#
gfortran -c hcell_steady.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hcell_steady.f"
  exit
fi
#
gfortran hcell_steady.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hcell_steady.o"
  exit
fi
rm hcell_steady.o
#
mv a.out ~/binf77/hcell_steady
#
echo "Executable installed as ~/binf77/hcell_steady"
