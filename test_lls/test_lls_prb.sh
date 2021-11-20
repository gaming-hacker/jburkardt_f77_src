#! /bin/bash
#
gfortran -c test_lls_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling test_lls_prb.f"
  exit
fi
#
gfortran test_lls_prb.o -L$HOME/libf77 -ltest_lls -lr8lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading test_lls_prb.o"
  exit
fi
rm test_lls_prb.o
#
mv a.out test_lls_prb
./test_lls_prb > test_lls_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running test_lls_prb"
  exit
fi
rm test_lls_prb
#
echo "Test program output written to test_lls_prb_output.txt."
