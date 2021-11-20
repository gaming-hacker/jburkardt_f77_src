#!/bin/bash
#
gfortran -c testpack.f
if [ $? -ne 0 ]; then
  echo "Errors compiling testpack.f"
  exit
fi
#
gfortran testpack.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading testpack.o"
  exit
fi
rm testpack.o
#
chmod ugo+x a.out
mv a.out ~/binf77/testpack
#
echo "Executable installed as ~/binf77/testpack"
