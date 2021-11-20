#!/bin/bash
#
gfortran -c matrix_multiply.f
if [ $? -ne 0 ]; then
  echo "Errors while compiling matrix_multiply.f"
  exit
fi
#
gfortran matrix_multiply.o
if [ $? -ne 0 ]; then
  echo "Errors while loading matrix_multiply.o"
  exit
fi
rm matrix_multiply.o
#
mv a.out ~/binf77/matrix_multiply
#
echo "Executable installed as ~/binf77/matrix_multiply"
