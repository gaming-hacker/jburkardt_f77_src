#!/bin/bash
#
gfortran -c hermite_exactness.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hermite_exactness.f"
  exit
fi
#
gfortran hermite_exactness.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hermite_exactness.o"
  exit
fi
rm hermite_exactness.o
#
chmod ugo+x a.out
mv a.out ~/binf77/hermite_exactness
#
echo "The program has been installed as ~/binf77/hermite_exactness."
