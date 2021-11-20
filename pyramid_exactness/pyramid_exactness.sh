#!/bin/bash
#
gfortran -c pyramid_exactness.f
if [ $? -ne 0 ]; then
  echo "Errors compiling pyramid_exactness.f"
  exit
fi
#
gfortran pyramid_exactness.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading pyramid_exactness.o"
  exit
fi
rm pyramid_exactness.o
#
chmod ugo+x a.out
mv a.out ~/binf77/pyramid_exactness
#
echo "Executable installed as ~/binf77/pyramid_exactness"
