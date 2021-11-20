#!/bin/bash
#
gfortran -c rbf_interp_2d_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling rbf_interp_2d_prb.f"
  exit
fi
#
gfortran rbf_interp_2d_prb.o -L$HOME/libf77 -lrbf_interp_2d -ltest_interp_2d -lr8lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading rbf_interp_2d_prb.o"
  exit
fi
rm rbf_interp_2d_prb.o
#
mv a.out rbf_interp_2d_prb
./rbf_interp_2d_prb > rbf_interp_2d_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running rbf_interp_2d_prb"
  exit
fi
rm rbf_interp_2d_prb
#
echo "Test program output written to rbf_interp_2d_prb_output.txt."
