#!/bin/bash
#
gfortran -c bisection_integer_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling bisection_integer_prb.f"
  exit
fi
#
gfortran bisection_integer_prb.o -L$HOME/libf77 -lbisection_integer
if [ $? -ne 0 ]; then
  echo "Errors linking and loading bisection_integer_prb.o"
  exit
fi
rm bisection_integer_prb.o
#
mv a.out bisection_integer_prb
./bisection_integer_prb > bisection_integer_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running bisection_integer_prb"
  exit
fi
rm bisection_integer_prb
#
echo "Program output written to bisection_integer_prb_output.txt"
