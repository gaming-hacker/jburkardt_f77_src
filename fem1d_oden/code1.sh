#!/bin/bash
#
gfortran -c code1.f
if [ $? -ne 0 ]; then
  echo "Errors compiling code1.f"
  exit
fi
#
gfortran code1.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading code1.o"
  exit
fi
rm code1.o
#
mv a.out ~/binf77/code1
#
echo "Executable installed as ~/binf77/code1"
