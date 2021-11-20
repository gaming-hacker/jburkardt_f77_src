#!/bin/bash
#
gfortran -c digits.f
if [ $? -ne 0 ]; then
  echo "Errors compiling digits.f"
  exit
fi
#
gfortran digits.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading digits.o"
  exit
fi
rm digits.o
#
mv a.out digits
./digits > digits_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running digits"
  exit
fi
rm digits
#
echo "Program output was written to digits_output.txt"
