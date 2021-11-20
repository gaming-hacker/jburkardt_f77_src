#!/bin/bash
#
gfortran -c test_mat_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling test_mat_prb.f"
  exit
fi
#
gfortran test_mat_prb.o -L$HOME/libf77 -ltest_mat
if [ $? -ne 0 ]; then
  echo "Errors linking and loading test_mat_prb.o"
  exit
fi
rm test_mat_prb.o
#
mv a.out test_mat_prb
./test_mat_prb > test_mat_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running test_mat_prb"
  exit
fi
rm test_mat_prb
#
echo "Test results written to test_mat_prb_output.txt."
