#!/bin/bash
#
gfortran -c fem_to_triangle.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fem_to_triangle.f"
  exit
fi
#
gfortran fem_to_triangle.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem_to_triangle.o"
  exit
fi
#
rm fem_to_triangle.o
#
chmod ugo+x a.out
mv a.out ~/binf77/fem_to_triangle
#
echo "Executable installed as ~/binf77/fem_to_triangle"
