#!/bin/bash
#
gfortran -c chkfmt1.f
if [ $? -ne 0 ]; then
  echo "Errors compiling chkfmt1.f"
  exit
fi
#
gfortran chkfmt1.o -L$HOME/libf77 -lsparsekit2
if [ $? -ne 0 ]; then
  echo "Errors linking and loading chkfmt1.o"
  exit
fi
rm chkfmt1.o
#
mv a.out chkfmt1
./chkfmt1 > chkfmt1_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running chkfmt1"
  exit
fi
rm chkfmt1
#
echo "Removing .MAT files created by program."
echo "You might wish to keep them, instead."
#
rm bnd.mat
rm coo.mat
rm csr.mat
rm dia.mat
rm dns.mat
rm itp.mat
rm jad.mat
rm msr.mat
#
echo "Test results written to chkfmt1_output.txt."
