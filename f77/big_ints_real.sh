#!/bin/bash
#
gfortran -c big_ints_real.f
if [ $? -ne 0 ]; then
  echo "Errors compiling big_ints_real.f"
  exit
fi
#
gfortran big_ints_real.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading big_ints_real.o"
  exit
fi
rm big_ints_real.o
#
mv a.out big_ints_real
./big_ints_real > big_ints_real_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running big_ints_real"
  exit
fi
rm big_ints_real
#
echo "Program output written to big_ints_real_output.txt"
