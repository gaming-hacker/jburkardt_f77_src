#!/bin/bash
#
gfortran -c test_min_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling test_min_prb.f"
  exit
fi
#
gfortran test_min_prb.o -L$HOME/libf77 -ltest_min
if [ $? -ne 0 ]; then
  echo "Errors linking and loading test_min_prb.o"
  exit
fi
rm test_min_prb.o
#
mv a.out test_min_prb
./test_min_prb > test_min_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running test_min_prb"
  exit
fi
rm test_min_prb
#
echo "Test program output written to test_min_prb_output.txt."
