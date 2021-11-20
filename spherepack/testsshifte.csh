#!/bin/csh
#
F77 -c -g testsshifte.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling testsshifte.f"
  exit
endif
rm compiler.txt
#
F77 testsshifte.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading testsshifte.o"
  exit
endif
rm testsshifte.o
#
mv a.out testsshifte
./testsshifte > testsshifte_output.txt
if ( $status != 0 ) then
  echo "Errors running testsshifte"
  exit
endif
rm testsshifte
#
echo "Test results written to testsshifte_output.txt."
