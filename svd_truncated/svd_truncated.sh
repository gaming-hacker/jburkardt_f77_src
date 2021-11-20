#!/bin/bash
#
gfortran -c svd_truncated.f
if [ $? -ne 0 ]; then
  echo "Errors compiling svd_truncated.f"
  exit
fi
#
#  Use this command normally:
#
gfortran svd_truncated.o -llapack
#
#  Use this command under the Macintosh OSX:
#
#gfortran svd_truncated.o -framework veclib
#
if [ $? -ne 0 ]; then
  echo "Errors linking and loading svd_truncated.o"
  exit
fi
rm svd_truncated.o
#
mv a.out svd_truncated
./svd_truncated > svd_truncated_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running svd_truncated"
  exit
fi
rm svd_truncated
#
echo "Program output written to svd_truncated_output.txt"
