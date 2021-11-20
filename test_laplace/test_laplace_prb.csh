#!/bin/csh
#
F77 -c -g test_laplace_prb.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling test_laplace_prb.f"
  exit
endif
rm compiler.txt
#
F77 test_laplace_prb.o -L$HOME/libf77/$ARCH -ltest_laplace
if ( $status != 0 ) then
  echo "Errors linking and loading test_laplace_prb.o"
  exit
endif
rm test_laplace_prb.o
#
mv a.out test_laplace_prb
./test_laplace_prb > test_laplace_prb_output.txt
if ( $status != 0 ) then
  echo "Errors running test_laplace_prb"
  exit
endif
rm test_laplace_prb
#
echo "Test results written to test_laplace_prb_output.txt."
