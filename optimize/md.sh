#!/bin/bash
#
gfortran -c md.f
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
mv a.out md
./md > md_output.txt
rm md
#
echo "Output written to md_output.txt"
