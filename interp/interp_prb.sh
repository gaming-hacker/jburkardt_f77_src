#!/bin/bash
#
gfortran -c interp_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling interp_prb.f"
  exit
fi
#
gfortran interp_prb.o -L$HOME/libf77 -linterp
if [ $? -ne 0 ]; then
  echo "Errors linking and loading interp_prb.o"
  exit
fi
rm interp_prb.o
#
mv a.out interp_prb
./interp_prb > interp_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running interp_prb"
  exit
fi
rm interp_prb
#
echo "Test program output written to interp_prb_output.txt."
