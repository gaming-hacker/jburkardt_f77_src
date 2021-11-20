#!/bin/bash
#
gfortran -c hregion_04.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hregion_04.f"
  exit
fi
#
gfortran hregion_04.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hregion_04.o"
  exit
fi
rm hregion_04.o
#
mv a.out ~/binf77/hregion_04
#
echo "A new version of hregion_04 has been created."
