#!/bin/bash
#
gfortran -c lagrange_approx_1d_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling lagrange_approx_1d_prb.f"
  exit
fi
#
gfortran lagrange_approx_1d_prb.o -L$HOME/libf77 -llagrange_approx_1d \
  -ltest_interp_1d -lqr_solve -lr8lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading lagrange_approx_1d_prb.o"
  exit
fi
rm lagrange_approx_1d_prb.o
#
mv a.out lagrange_approx_1d_prb
./lagrange_approx_1d_prb > lagrange_approx_1d_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running lagrange_approx_1d_prb"
  exit
fi
rm lagrange_approx_1d_prb
#
echo "Test program output written to lagrange_approx_1d_prb_output.txt."
