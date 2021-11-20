#!/bin/bash
#
gfortran -c -g test_interp_fun_prb.f >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling test_interp_fun_prb.f"
  exit
fi
rm compiler.txt
#
gfortran test_interp_fun_prb.o -L$HOME/libf77 -ltest_interp_fun -lspline
if [ $? -ne 0 ]; then
  echo "Errors linking and loading test_interp_fun_prb.o"
  exit
fi
rm test_interp_fun_prb.o
#
mv a.out test_interp_fun_prb
./test_interp_fun_prb > test_interp_fun_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running test_interp_fun_prb"
  exit
fi
rm test_interp_fun_prb
#
echo "Test program output written to test_interp_fun_prb_output.txt."
