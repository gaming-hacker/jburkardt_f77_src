#!/bin/bash
#
gfortran -c -Wall kmedian.f
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran kmedian.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm kmedian.o
#
mv a.out ~/binf77/kmedian
#
echo "Normal end of execution."
