#!/bin/bash
#
gfortran -c code2.f
if [ $? -ne 0 ]; then
  echo "Errors compiling code2.f"
  exit
fi
#
gfortran code2.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading code2.o"
  exit
fi
rm code2.o
#
mv a.out ~/binf77/code2
#
echo "Executable installed as ~/binf77/code2"
