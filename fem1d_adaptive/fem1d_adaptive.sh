#!/bin/bash
#
gfortran -c fem1d_adaptive.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fem1d_adaptive.f"
  exit
fi
#
gfortran fem1d_adaptive.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem1d_adaptive.o"
  exit
fi
rm fem1d_adaptive.o
#
chmod ugo+x a.out
mv a.out ~/binf77/fem1d_adaptive
#
echo "Executable installed as ~/binf77/fem1d_adaptive"
