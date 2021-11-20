#!/bin/bash
#
gfortran -c division.f
if [ $? -ne 0 ]; then
  echo "Errors compiling division.f"
  exit
fi
#
gfortran division.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading division.o"
  exit
fi
rm division.o
#
mv a.out division
./division > division_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running division"
  exit
fi
rm division
#
echo "Program output written to division_output.txt"
