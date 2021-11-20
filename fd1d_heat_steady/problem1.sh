#!/bin/bash
#
gfortran -c problem1.f
if [ $? -ne 0 ]; then
  echo "Errors compiling problem1.f"
  exit
fi
#
gfortran problem1.o -L$HOME/libf77 -lfd1d_heat_steady
if [ $? -ne 0 ]; then
  echo "Errors linking and loading problem1.o"
  exit
fi
rm problem1.o
#
mv a.out problem1
./problem1 > problem1_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running problem1"
  exit
fi
rm problem1
#
echo "Test program output written to problem1_output.txt."
