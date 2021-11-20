#!/bin/bash
#
gfortran -c circle_inout.f
if [ $? -ne 0 ]; then
  echo "Errors compiling circle_inout.f"
  exit
fi
#
gfortran circle_inout.o -L/usr/local/dislin -ldislin -L/opt/local/lib -lXm
if [ $? -ne 0 ]; then
  echo "Errors linking and loading circle_inout.o."
  exit
fi
#
rm circle_inout.o
#
mv a.out circle_inout
./circle_inout > circle_inout_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running circle_inout."
  exit
fi
rm circle_inout
#
echo "Program output written to circle_inout_output.txt"
