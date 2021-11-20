#!/bin/bash
#
gfortran -c toms431.f
if [ $? -ne 0 ]; then
  echo "Errors while compiling toms431.f"
  exit
fi
#
gfortran toms431.o
if [ $? -ne 0 ]; then
  echo "Errors while loading toms431.o"
  exit
fi
rm toms431.o
#
mv a.out ~/binf77/toms431
#
echo "Executable installed as ~/binf77/toms431"
