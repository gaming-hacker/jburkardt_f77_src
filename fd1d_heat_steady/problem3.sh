#!/bin/bash
#
gfortran -c problem3.f
if [ $? -ne 0 ]; then
  echo "Errors compiling problem3.f"
  exit
fi
#
gfortran problem3.o -L$HOME/libf77 -lfd1d_heat_steady
if [ $? -ne 0 ]; then
  echo "Errors linking and loading problem3.o"
  exit
fi
rm problem3.o
#
mv a.out problem3
./problem3 > problem3_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running problem3"
  exit
fi
rm problem3
#
echo "Test program output written to problem3_output.txt."
