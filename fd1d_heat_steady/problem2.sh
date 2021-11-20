#!/bin/bash
#
gfortran -c problem2.f
if [ $? -ne 0 ]; then
  echo "Errors compiling problem2.f"
  exit
fi
#
gfortran problem2.o -L$HOME/libf77 -lfd1d_heat_steady
if [ $? -ne 0 ]; then
  echo "Errors linking and loading problem2.o"
  exit
fi
rm problem2.o
#
mv a.out problem2
./problem2 > problem2_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running problem2"
  exit
fi
rm problem2
#
echo "Test program output written to problem2_output.txt."
