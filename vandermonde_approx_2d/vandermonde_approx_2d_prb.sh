#!/bin/bash
#
gfortran -c -g vandermonde_approx_2d_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling vandermonde_approx_2d_prb.f"
  exit
fi
#
gfortran vandermonde_approx_2d_prb.o -L$HOME/libf77 -lvandermonde_approx_2d \
  -ltest_interp_2d -lqr_solve -lr8lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading vandermonde_approx_2d_prb.o"
  exit
fi
rm vandermonde_approx_2d_prb.o
#
mv a.out vandermonde_approx_2d_prb
./vandermonde_approx_2d_prb > vandermonde_approx_2d_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running vandermonde_approx_2d_prb"
  exit
fi
rm vandermonde_approx_2d_prb
#
echo "Test program output written to vandermonde_approx_2d_prb_output.txt."
