#!/bin/bash
#
gfortran -c fem_to_medit.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fem_to_medit.f"
  exit
fi
#
gfortran fem_to_medit.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem_to_medit.o"
  exit
fi
#
rm fem_to_medit.o
#
chmod ugo+x a.out
mv a.out ~/binf77/fem_to_medit
#
echo "Program installed as ~/binf77/fem_to_medit"
