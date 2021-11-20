#!/bin/bash
#
gfortran -c hcell.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hcell.f"
  exit
fi
#
gfortran hcell.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hcell.o"
  exit
fi
rm hcell.o
#
mv a.out ~/binf77/hcell
#
echo "Executable installed as ~/binf77/hcell."
