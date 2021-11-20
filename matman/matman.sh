#!/bin/bash
#
gfortran -c matman.f
if [ $? -ne 0 ]; then
  echo "Errors compiling matman.f"
  exit
fi
#
gfortran matman.o -lblas
if [ $? -ne 0 ]; then
  echo "Errors linking and loading matman.o"
  exit
fi
rm matman.o
#
chmod ugo+x a.out
mv a.out ~/binf77/matman
#
echo "Executable installed as ~/binf77/matman"
