#!/bin/bash
#
gfortran -c tsp_brute.f
if [ $? -ne 0 ]; then
  echo "Errors compiling tsp_brute.f"
  exit
fi
#
gfortran tsp_brute.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading tsp_brute.o"
  exit
fi
rm tsp_brute.o
#
mv a.out ~/binf77/tsp_brute
#
echo "Executable installed as ~/binf77/tsp_brute"
