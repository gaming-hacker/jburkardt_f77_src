#!/bin/bash
#
gfortran -c constant_type.f
if [ $? -ne 0 ]; then
  echo "Errors compiling constant_type.f"
  exit
fi
#
gfortran constant_type.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading constant_type.o"
  exit
fi
rm constant_type.o
#
mv a.out constant_type
./constant_type > constant_type_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running constant_type"
  exit
fi
rm constant_type
#
echo "Program output written to constant_type_output.txt"
