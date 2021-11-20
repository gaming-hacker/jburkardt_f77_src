#!/bin/bash
#
gfortran -c test_con_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling test_con_prb.f"
  exit
fi
#
gfortran test_con_prb.o -L$HOME/libf77 -ltest_con
if [ $? -ne 0 ]; then
  echo "Errors linking and loading test_con_prb.o"
  exit
fi
rm test_con_prb.o
#
mv a.out test_con_prb
./test_con_prb > test_con_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running test_con_prb"
  exit
fi
rm test_con_prb
#
echo "Test results written to test_con_prb_output.txt."
