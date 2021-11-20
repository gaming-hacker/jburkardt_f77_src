#!/bin/bash
#
gfortran -c gfplys.f
if [ $? -ne 0 ]; then
  echo "Error while compiling gfplys.f"
  exit
fi
#
gfortran gfplys.o
if [ $? -ne 0 ]; then
  echo "Error while loading gfplys.o"
  exit
fi
rm gfplys.o
#
mv a.out ~/binf77/gfplys
#
echo "Executable installed as ~/binf77/gflys"
