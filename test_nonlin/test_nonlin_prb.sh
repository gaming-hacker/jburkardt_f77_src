#!/bin/bash
#
gfortran -c test_nonlin_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling test_nonlin_prb.f"
  exit
fi
#
gfortran test_nonlin_prb.o -L$HOME/libf77 -ltest_nonlin
if [ $? -ne 0 ]; then
  echo "Errors linking and loading test_nonlin_prb.o"
  exit
fi
rm test_nonlin_prb.o
#
mv a.out test_nonlin_prb
./test_nonlin_prb > test_nonlin_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running test_nonlin_prb"
  exit
fi
rm test_nonlin_prb
#
echo "Test results written to test_nonlin_prb_output.txt."
