#!/bin/bash
#
gfortran -c test_opt_con_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling test_opt_con_prb.f"
  exit
fi
#
gfortran test_opt_con_prb.o -L$HOME/libf77 -ltest_opt_con
if [ $? -ne 0 ]; then
  echo "Errors linking and loading test_opt_con_prb.o"
  exit
fi
rm test_opt_con_prb.o
#
mv a.out test_opt_con_prb
./test_opt_con_prb > test_opt_con_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running test_opt_con_prb"
  exit
fi
rm test_opt_con_prb
#
echo "Test program output written to test_opt_con_prb_output.txt."
