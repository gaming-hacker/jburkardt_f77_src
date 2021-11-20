#!/bin/bash
#
gfortran -c -g distances.f >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling distances.f"
  exit
fi
rm compiler.txt
#
gfortran distances.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading distances.o"
  exit
fi
rm distances.o
#
chmod ugo+x a.out
mv a.out ~/binf77/distances
#
echo "Program installed as ~/binf77/distances"
