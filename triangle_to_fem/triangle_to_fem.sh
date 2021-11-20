#!/bin/bash
#
gfortran -c triangle_to_fem.f
if [ $? -ne 0 ]; then
  echo "Errors compiling triangle_to_fem.f"
  exit
fi
#
gfortran triangle_to_fem.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading triangle_to_fem.o"
  exit
fi
#
rm triangle_to_fem.o
#
chmod ugo+x a.out
mv a.out ~/binf77/triangle_to_fem
#
echo "Executable installed as ~/binf77/triangle_to_fem"
