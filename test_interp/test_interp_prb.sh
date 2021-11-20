#!/bin/bash
#
gfortran -c test_interp_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling test_interp_prb.f"
  exit
fi
#
gfortran test_interp_prb.o -L$HOME/libf77 -ltest_interp -lr8lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading test_interp_prb.o"
  exit
fi
rm test_interp_prb.o
#
mv a.out test_interp_prb
./test_interp_prb > test_interp_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running test_interp_prb"
  exit
fi
rm test_interp_prb
#
echo "Test program output written to test_interp_prb_output.txt."
