#!/bin/bash
#
gfortran -c test_interp_nd_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling test_interp_nd_prb.f"
  exit
fi
#
gfortran test_interp_nd_prb.o -L$HOME/libf77 -ltest_interp_nd -lr8lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading test_interp_nd_prb.o"
  exit
fi
rm test_interp_nd_prb.o
#
mv a.out test_interp_nd_prb
./test_interp_nd_prb > test_interp_nd_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running test_interp_nd_prb"
  exit
fi
rm test_interp_nd_prb
#
echo "Test program output written to test_interp_nd_prb_output.txt."
