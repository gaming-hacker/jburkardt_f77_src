#!/bin/csh
#
F77 -c -g tvlap.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling tvlap.f"
  exit
endif
rm compiler.txt
#
F77 tvlap.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading tvlap.o"
  exit
endif
rm tvlap.o
#
mv a.out tvlap
./tvlap > tvlap_output.txt
if ( $status != 0 ) then
  echo "Errors running tvlap"
  exit
endif
rm tvlap
#
echo "Test results written to tvlap_output.txt."
