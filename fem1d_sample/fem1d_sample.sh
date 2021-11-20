#!/bin/bash
#
gfortran -c fem1d_sample.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fem1d_sample.f90"
  exit
fi
#
gfortran fem1d_sample.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem1d_sample.o"
  exit
fi
#
rm fem1d_sample.o
#
chmod ugo+x a.out
mv a.out ~/binf77/fem1d_sample
#
echo "Program installed as ~/binf77/fem1d_sample"
