#!/bin/bash
#
gfortran -c laguerre_exactness.f
if [ $? -ne 0 ]; then
  echo "Errors compiling laguerre_exactness.f"
  exit
fi
#
gfortran laguerre_exactness.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading laguerre_exactness.o"
  exit
fi
rm laguerre_exactness.o
#
chmod ugo+x a.out
mv a.out ~/binf77/laguerre_exactness
#
echo "The program has been installed as ~/binf77/laguerre_exactness."
