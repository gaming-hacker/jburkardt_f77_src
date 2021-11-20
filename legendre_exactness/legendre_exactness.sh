#!/bin/bash
#
gfortran -c legendre_exactness.f
if [ $? -ne 0 ]; then
  echo "Errors compiling legendre_exactness.f"
  exit
fi
#
gfortran legendre_exactness.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading legendre_exactness.o"
  exit
fi
rm legendre_exactness.o
#
chmod ugo+x a.out
mv a.out ~/binf77/legendre_exactness
#
echo "The program has been installed as ~/binf77/legendre_exactness."
