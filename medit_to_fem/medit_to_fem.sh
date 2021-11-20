#!/bin/bash
#
gfortran -c medit_to_fem.f
if [ $? -ne 0 ]; then
  echo "Errors compiling medit_to_fem.f"
  exit
fi
#
gfortran medit_to_fem.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading medit_to_fem.o"
  exit
fi
#
rm medit_to_fem.o
#
chmod ugo+x a.out
mv a.out ~/binf77/medit_to_fem
#
echo "Executable installed as ~/binf77/medit_to_fem"
