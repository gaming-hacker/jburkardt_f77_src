#!/bin/bash
#
gfortran -c hregion_05.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hregion_05.f"
  exit
fi
#
gfortran hregion_05.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hregion_05.o"
  exit
fi
rm hregion_05.o
#
mv a.out ~/binf77/hregion_05
#
echo "A new version of hregion_05 has been created."
