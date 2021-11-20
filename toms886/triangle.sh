#!/bin/bash
#
gfortran -c triangle.f
if [ $? -ne 0 ]; then
  echo "Errors compiling triangle.f"
  exit
fi
#
gfortran triangle.o -L$HOME/libf77 -ltoms886
if [ $? -ne 0 ]; then
  echo "Errors linking and loading triangle.o"
  exit
fi
rm triangle.o
#
mv a.out triangle
./triangle > triangle_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running triangle"
  exit
fi
rm triangle
#
echo "Test results written to triangle_output.txt."
