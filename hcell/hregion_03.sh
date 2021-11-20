#!/bin/bash
#
gfortran -c hregion_03.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hregion_03.f"
  exit
fi
#
gfortran hregion_03.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hregion_03.o"
  exit
fi
rm hregion_03.o
#
mv a.out ~/binf77/hregion_03
#
echo "A new version of hregion_03 has been created."
