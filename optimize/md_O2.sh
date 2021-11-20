#!/bin/bash
#
gfortran -c -O2 md.f
if [ $? -ne 0 ]; then
  echo "Errors compiling md.f"
  exit
fi
#
gfortran md.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading md.o"
  exit
fi
rm md.o
#
mv a.out md_O2
./md_O2 > md_O2_output.txt
rm md_O2
#
echo "Output written to md_O2_output.txt"
