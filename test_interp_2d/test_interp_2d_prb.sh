#!/bin/bash
#
gfortran -c test_interp_2d_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling test_interp_2d_prb.f"
  exit
fi
#
gfortran test_interp_2d_prb.o -L$HOME/libf77 -ltest_interp_2d -lr8lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading test_interp_2d_prb.o"
  exit
fi
rm test_interp_2d_prb.o
#
mv a.out test_interp_2d_prb
./test_interp_2d_prb > test_interp_2d_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running test_interp_2d_prb"
  exit
fi
rm test_interp_2d_prb
#
echo "Program output written to test_interp_2d_prb_output.txt"
