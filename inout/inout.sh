#!/bin/bash
#
gfortran -c inout.f
if [ $? -ne 0 ]; then
  echo "Errors compiling inout.f"
  exit
fi
#
gfortran inout.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading inout.o"
  exit
fi
rm inout.o
#
chmod ugo+x a.out
mv a.out ~/binf77/inout
#
echo "Executable installed as ~/binf77/inout"
