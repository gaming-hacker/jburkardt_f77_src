#!/bin/bash
#
gfortran -c bisection_rc_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling bisection_rc_prb.f"
  exit
fi
#
gfortran bisection_rc_prb.o -L$HOME/libf77 -lbisection_rc
if [ $? -ne 0 ]; then
  echo "Errors linking and loading bisection_rc_prb.o"
  exit
fi
rm bisection_rc_prb.o
#
mv a.out bisection_rc_prb
./bisection_rc_prb > bisection_rc_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running bisection_rc_prb"
  exit
fi
rm bisection_rc_prb
#
echo "Test program output written to bisection_rc_prb_output.txt."
