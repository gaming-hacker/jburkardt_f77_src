#!/bin/bash
#
gfortran -c genin.f
if [ $? -ne 0 ]; then
  echo "Error while compiling genin.f"
  exit
fi
#
gfortran genin.o
if [ $? -ne 0 ]; then
  echo "Error while loading genin.o"
  exit
fi
rm genin.o
#
mv a.out ~/binf77/genin
#
echo "Executable installed as ~/binf77/genin"
