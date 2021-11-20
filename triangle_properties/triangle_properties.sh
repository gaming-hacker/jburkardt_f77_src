#!/bin/bash
#
gfortran -c triangle_properties.f
if [ $? -ne 0 ]; then
  echo "Errors compiling triangle_properties.f"
  exit
fi
#
gfortran triangle_properties.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading triangle_properties.o"
  exit
fi
rm triangle_properties.o
#
chmod ugo+x a.out
mv a.out ~/binf77/triangle_properties
#
echo "Executable installed as ~/binf77/triangle_properties"
