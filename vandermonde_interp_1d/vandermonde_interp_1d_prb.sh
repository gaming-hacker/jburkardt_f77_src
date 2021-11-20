#!/bin/bash
#
gfortran -c -g vandermonde_interp_1d_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling vandermonde_interp_1d_prb.f"
  exit
fi
#
gfortran vandermonde_interp_1d_prb.o -L$HOME/libf77 -lvandermonde_interp_1d \
  -lqr_solve -lcondition -ltest_interp -lr8lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading vandermonde_interp_1d_prb.o"
  exit
fi
rm vandermonde_interp_1d_prb.o
#
mv a.out vandermonde_interp_1d_prb
./vandermonde_interp_1d_prb > vandermonde_interp_1d_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running vandermonde_interp_1d_prb"
  exit
fi
rm vandermonde_interp_1d_prb
#
echo "Test program output written to vandermonde_interp_1d_prb_output.txt."
