#!/bin/bash
#
gfortran -c exponent_format_overflow.f
if [ $? -ne 0 ]; then
  echo "Errors compiling exponent_format_overflow.f"
  exit
fi
#
gfortran exponent_format_overflow.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading exponent_format_overflow.o"
  exit
fi
rm exponent_format_overflow.o
#
mv a.out exponent_format_overflow
./exponent_format_overflow > exponent_format_overflow_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running exponent_format_overflow"
  exit
fi
rm exponent_format_overflow
#
echo "Program output written to exponent_format_overflow_output.txt"
