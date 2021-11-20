#!/bin/bash
#
gfortran -c mxm.f
if [ $? -ne 0 ]; then
  echo "Errors compiling mxm.f"
  exit
fi
#
gfortran mxm.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading mxm.o"
  exit
fi
rm mxm.o
#
mv a.out ~/binf77/mxm
#
echo "Executable installed as ~/binf77/mxm"
