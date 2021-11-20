#!/bin/bash
#
gfortran -c fem2d_poisson_rectangle.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fem2d_poisson_rectangle.f"
  exit
fi
#
gfortran fem2d_poisson_rectangle.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem2d_poisson_rectangle.o"
  exit
fi
rm fem2d_poisson_rectangle.o
#
chmod ugo+x a.out
mv a.out ~/binf77/fem2d_poisson_rectangle
#
echo "Executable installed as ~/binf77/fem2d_poisson_rectangle"
