#!/bin/bash
#
gfortran -c double_complex.f
if [ $? -ne 0 ]; then
  echo "Errors compiling double_complex.f"
  exit
fi
#
gfortran double_complex.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading double_complex.o"
  exit
fi
rm double_complex.o
#
mv a.out double_complex
./double_complex > double_complex_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running double_complex"
  exit
fi
rm double_complex
#
echo "Program output written to double_complex_output.txt"
