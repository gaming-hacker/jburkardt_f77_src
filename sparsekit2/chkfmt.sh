#!/bin/bash
#
gfortran -c chkfmt.f
if [ $? -ne 0 ]; then
  echo "Errors compiling chkfmt.f"
  exit
fi
#
gfortran chkfmt.o -L$HOME/libf77 -lsparsekit2
if [ $? -ne 0 ]; then
  echo "Errors linking and loading chkfmt.o"
  exit
fi
rm chkfmt.o
#
mv a.out chkfmt
./chkfmt > chkfmt_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running chkfmt"
  exit
fi
rm chkfmt
#
echo ""
echo "Removing file unary.mat.  You might prefer to keep and examine it."
rm unary.mat
#
echo "Test results written to chkfmt_output.txt."
