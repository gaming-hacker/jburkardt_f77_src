#!/bin/bash
#
gfortran -c lagrange_interp_2d_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling lagrange_interp_2d_prb.f"
  exit
fi
#
gfortran lagrange_interp_2d_prb.o -L$HOME/libf77 -llagrange_interp_2d -ltest_interp_2d -lr8lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading lagrange_interp_2d_prb.o"
  exit
fi
rm lagrange_interp_2d_prb.o
#
mv a.out lagrange_interp_2d_prb
./lagrange_interp_2d_prb > lagrange_interp_2d_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running lagrange_interp_2d_prb"
  exit
fi
rm lagrange_interp_2d_prb
#
echo "Test program output written to lagrange_interp_2d_prb_output.txt."
