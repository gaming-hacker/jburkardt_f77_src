#!/bin/bash
#
gfortran -c toeplitz_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toeplitz_prb.f"
  exit
fi
#
gfortran toeplitz_prb.o -L$HOME/libf77 -ltoeplitz
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toeplitz_prb.o"
  exit
fi
rm toeplitz_prb.o
#
mv a.out toeplitz_prb
./toeplitz_prb > toeplitz_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toeplitz_prb"
  exit
fi
rm toeplitz_prb
#
echo "Test results written to toeplitz_prb_output.txt."
