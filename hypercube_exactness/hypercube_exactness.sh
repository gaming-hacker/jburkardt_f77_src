#!/bin/bash
#
gfortran -c hypercube_exactness.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hypercube_exactness.f"
  exit
fi
#
gfortran hypercube_exactness.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hypercube_exactness.o"
  exit
fi
rm hypercube_exactness.o
#
chmod ugo+x a.out
mv a.out ~/binf77/hypercube_exactness
#
echo "Executable installed as ~/binf77/hypercube_exactness"
