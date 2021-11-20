#!/bin/bash
#
gfortran -c -g st_to_dsp.f >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling st_to_dsp.f"
  exit
fi
rm compiler.txt
#
gfortran st_to_dsp.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading st_to_dsp.o"
  exit
fi
rm st_to_dsp.o
#
chmod ugo+x a.out
mv a.out ~/binf77/st_to_dsp
#
echo "Executable installed as ~/binf77/st_to_dsp"
