#!/bin/bash
#
gfortran -c bvec_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling bvec_prb.f"
  exit
fi
#
gfortran -o bvec_prb bvec_prb.o -L$HOME/libf77 -lbvec
if [ $? -ne 0 ]; then
  echo "Errors linking and loading bvec_prb.o"
  exit
fi
rm bvec_prb.o
#
./bvec_prb > bvec_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running bvec_prb"
  exit
fi
rm bvec_prb
#
echo "Test results written to bvec_prb_output.txt."
