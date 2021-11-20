#!/bin/bash
#
gfortran -c gfortran_quadmath.f
if [ $? -ne 0 ]; then
  echo "Errors compiling gfortran_quadmath.f"
  exit
fi
#
gfortran gfortran_quadmath.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading gfortran_quadmath.o"
  exit
fi
rm gfortran_quadmath.o
#
mv a.out gfortran_quadmath
./gfortran_quadmath > gfortran_quadmath_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running gfortran_quadmath"
# exit
fi
rm gfortran_quadmath
#
echo "Program output written to gfortran_quadmath_output.txt."
