#!/bin/bash
#
gfortran -c bvpsol_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling bvpsol_prb.f"
  exit
fi
#
gfortran bvpsol_prb.o -L$HOME/libf77 -lbvpsol
if [ $? -ne 0 ]; then
  echo "Errors linking and loading bvpsol_prb.o"
  exit
fi
rm bvpsol_prb.o
#
mv a.out bvpsol_prb
./bvpsol_prb > bvpsol_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running bvpsol_prb"
  exit
fi
rm bvpsol_prb
#
echo "Test results written to bvpsol_prb_output.txt."
