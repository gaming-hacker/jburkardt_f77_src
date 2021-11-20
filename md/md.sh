#!/bin/bash
#
gfortran -c -O2 md.f
if [ $? -ne 0 ]; then
  echo "Errors compiling md.f"
  exit
fi
#
gfortran md.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading md.o"
  exit
fi
rm md.o
#
mv a.out ~/binf77/md
#
echo "Program installed as ~/binf77/md"
