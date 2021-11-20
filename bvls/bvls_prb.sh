#!/bin/bash
#
gfortran -c bvls_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling bvls_prb.f"
  exit
fi
#
gfortran -o bvls_prb bvls_prb.o -L$HOME/libf77 -lbvls
if [ $? -ne 0 ]; then
  echo "Errors linking and loading bvls_prb.o"
  exit
fi
rm bvls_prb.o
#
./bvls_prb > bvls_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running bvls_prb"
  exit
fi
rm bvls_prb
#
echo "Test results written to bvls_prb_output.txt."
