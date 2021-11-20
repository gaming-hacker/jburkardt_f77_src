#!/bin/bash
#
gfortran -c sphere_exactness.f
if [ $? -ne 0 ]; then
  echo "Errors compiling sphere_exactness.f"
  exit
fi
#
gfortran sphere_exactness.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading sphere_exactness.o"
  exit
fi
rm sphere_exactness.o
#
chmod ugo+x a.out
mv a.out ~/binf77/sphere_exactness
#
echo "Program installed as ~/binf77/sphere_exactness"
