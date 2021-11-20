#!/bin/bash
#
gfortran -c matmul.f
if [ $? -ne 0 ]; then
  echo "Errors compiling matmul.f"
  exit
fi
#
gfortran matmul.o -lblas
if [ $? -ne 0 ]; then
  echo "Errors linking and loading matmul.o"
  exit
fi
rm matmul.o
#
chmod ugo+x a.out
mv a.out ~/binf77/matmul
#
echo "Executable installed as ~/binf77/matmul"
