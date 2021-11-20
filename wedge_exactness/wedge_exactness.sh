#!/bin/bash
#
gfortran -c wedge_exactness.f
if [ $? -ne 0 ]; then
  echo "Errors compiling wedge_exactness.f"
  exit
fi
#
gfortran wedge_exactness.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading wedge_exactness.o"
  exit
fi
rm wedge_exactness.o
#
chmod ugo+x a.out
mv a.out ~/binf77/wedge_exactness
#
echo "Executable installed as ~/binf77/wedge_exactness"
