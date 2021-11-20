#!/bin/bash
#
gfortran -c damped_sine.f
if [ $? -ne 0 ]; then
  echo "Errors compiling damped_sine.f"
  exit
fi
#
gfortran damped_sine.o
if [ $? -ne 0 ]; then
  echo "Errors linking damped_sine.o"
  exit
fi
#
rm damped_sine.o
mv a.out ~/binf77/damped_sine
#
echo "Executable installed as ~/binf77/damped_sine"
