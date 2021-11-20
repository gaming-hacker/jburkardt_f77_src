#!/bin/bash
#
gfortran -c laguerre_test_int_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling laguerre_test_int_prb.f"
  exit
fi
#
gfortran laguerre_test_int_prb.o -L$HOME/libf77 -llaguerre_test_int
if [ $? -ne 0 ]; then
  echo "Errors linking and loading laguerre_test_int_prb.o"
  exit
fi
rm laguerre_test_int_prb.o
#
mv a.out laguerre_test_int_prb
./laguerre_test_int_prb > laguerre_test_int_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running laguerre_test_int_prb"
  exit
fi
rm laguerre_test_int_prb
#
echo "Test program output written to laguerre_test_int_prb_output.txt."
