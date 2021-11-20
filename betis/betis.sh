#!/bin/bash
#
gfortran -c betis.f
if [ $? -ne 0 ]; then
  echo "Errors compiling betis.f"
  exit
fi
#
gfortran betis.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading betis.o"
  exit
fi
rm betis.o
#
mv a.out ~/binf77/betis
#
echo "Executable installed as ~/binf77/betis."
