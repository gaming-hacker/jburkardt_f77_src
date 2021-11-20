#!/bin/bash
#
gfortran -c genin2.f
if [ $? -ne 0 ]; then
  echo "Error while compiling genin2.f"
  exit
fi
#
gfortran genin2.o
if [ $? -ne 0 ]; then
  echo "Error while loading genin2.o"
  exit
fi
rm genin2.o
#
mv a.out ~/binf77/genin2
#
echo "Executable installed as ~/binf77/genin2"
