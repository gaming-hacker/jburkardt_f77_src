#!/bin/bash
#
gfortran -c problem4.f
if [ $? -ne 0 ]; then
  echo "Errors compiling problem4.f"
  exit
fi
#
gfortran problem4.o -L$HOME/libf77 -lfd1d_heat_steady
if [ $? -ne 0 ]; then
  echo "Errors linking and loading problem4.o"
  exit
fi
rm problem4.o
#
mv a.out problem4
./problem4 > problem1_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running problem4"
  exit
fi
rm problem4
#
echo "Test program output written to problem4_output.txt."
