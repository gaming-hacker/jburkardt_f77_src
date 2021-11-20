#!/bin/bash
#
gfortran -c diaphony.f
if [ $? -ne 0 ]; then
  echo "Errors compiling diaphony.f"
  exit
fi
#
gfortran diaphony.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading diaphony.o"
  exit
fi
#
rm diaphony.o
#
chmod ugo+x a.out
mv a.out ~/binf77/diaphony
#
echo "Executable installed as ~/binf77/diaphony"
