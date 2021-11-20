#!/bin/bash
#
gfortran -c f77_return.f
if [ $? -ne 0 ]; then
  echo "Errors while compiling f77_return.f"
  exit
fi
#
gfortran f77_return.o
if [ $? -ne 0 ]; then
  echo "Errors while loading f77_return.o"
  exit
fi
rm f77_return.o
#
mv a.out f77_return
f77_return > f77_return_output.txt
if [ $? -eq 13 ]; then
  echo "ERROR!  The computed sum exceeded 250."
  exit
fi
if [ $? -eq 99 ]; then
  echo "ERROR!  The computed sum was less than 1000."
  exit
fi
rm f77_return
#
echo "Example output written to f77_return_output.txt"
