#!/bin/bash
#
gfortran -c test_int_2d_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling test_int_2d_prb.f"
  exit
fi
#
gfortran test_int_2d_prb.o -L$HOME/libf77 -ltest_int_2d
if [ $? -ne 0 ]; then
  echo "Errors linking and loading test_int_2d_prb.o"
  exit
fi
rm test_int_2d_prb.o
#
mv a.out test_int_2d_prb
./test_int_2d_prb > test_int_2d_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running test_int_2d_prb"
  exit
fi
rm test_int_2d_prb
#
echo "Test program output written to test_int_2d_prb_output.txt."
