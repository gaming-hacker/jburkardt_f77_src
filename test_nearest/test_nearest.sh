#!/bin/bash
#
gfortran -c test_nearest.f
if [ $? -ne 0 ]; then
  echo "Errors compiling test_nearest.f"
  exit
fi
#
gfortran test_nearest.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading test_nearest.o"
  exit
fi
rm test_nearest.o
#
chmod ugo+x a.out
mv a.out ~/binf77/test_nearest
#
echo "Executable installed as ~/binf77/test_nearest"
