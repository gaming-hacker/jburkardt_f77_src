#!/bin/bash
#
gfortran -c mandelbrot.f
if [ $? -ne 0 ]; then
  echo "Errors compiling mandelbrot.f"
  exit
fi
#
gfortran mandelbrot.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading mandelbrot.o"
  exit
fi
rm mandelbrot.o
#
mv a.out ~/binf77/mandelbrot
#
echo "Executable installed as ~/binf77/mandelbrot"
