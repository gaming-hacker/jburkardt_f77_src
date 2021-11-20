#!/bin/bash
#
gfortran -c gfarit.f
if [ $? -ne 0 ]; then
  echo "Error while compiling gfarit.f"
  exit
fi
#
gfortran gfarit.o
if [ $? -ne 0 ]; then
  echo "Error while loading gfarit.o"
  exit
fi
rm gfarit.o
#
mv a.out ~/binf77/gfarit
#
echo "Executable installed as ~/binf77/gfarit"
