#!/bin/bash
#
gfortran -c randlc_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling randlc_prb.f"
  exit
fi
#
gfortran randlc_prb.o -L$HOME/libf77 -lrandlc
if [ $? -ne 0 ]; then
  echo "Errors linking and loading randlc_prb.o"
  exit
fi
rm randlc_prb.o
#
mv a.out randlc_prb
./randlc_prb > randlc_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running randlc_prb"
  exit
fi
rm randlc_prb
#
echo "Test results written to randlc_prb_output.txt."
