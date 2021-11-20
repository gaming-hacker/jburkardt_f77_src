#!/bin/bash
#
gfortran -c fem1d_bvp_linear_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fem1d_bvp_linear_prb.f"
  exit
fi
#
gfortran fem1d_bvp_linear_prb.o -L$HOME/libf77 -lfem1d_bvp_linear
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem1d_bvp_linear_prb.o"
  exit
fi
rm fem1d_bvp_linear_prb.o
#
mv a.out fem1d_bvp_linear_prb
./fem1d_bvp_linear_prb > fem1d_bvp_linear_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running fem1d_bvp_linear_prb"
  exit
fi
rm fem1d_bvp_linear_prb
#
echo "Program output written to fem1d_bvp_linear_prb_output.txt"
