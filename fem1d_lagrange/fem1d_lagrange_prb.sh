#!/bin/bash
#
gfortran -c fem1d_lagrange_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fem1d_lagrange_prb.f"
  exit
fi
#
gfortran -o fem1d_lagrange_prb fem1d_lagrange_prb.o -L$HOME/libf77 -lfem1d_lagrange
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem1d_lagrange_prb.o"
  exit
fi
rm fem1d_lagrange_prb.o
#
./fem1d_lagrange_prb > fem1d_lagrange_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running fem1d_lagrange_prb"
  exit
fi
rm fem1d_lagrange_prb
#
echo "Test program output written to fem1d_lagrange_prb_output.txt."
