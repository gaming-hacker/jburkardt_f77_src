#!/bin/bash
#
gfortran -c rvbr.f
if [ $? -ne 0 ]; then
  echo "Errors compiling rvbr.f"
  exit
fi
#
gfortran rvbr.o -L$HOME/libf77 -lsparsekit2
if [ $? -ne 0 ]; then
  echo "Errors linking and loading rvbr.o"
  exit
fi
rm rvbr.o
#
mv a.out rvbr
./rvbr > rvbr_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running rvbr"
  exit
fi
rm rvbr
#
echo "Test results written to rvbr_output.txt."
