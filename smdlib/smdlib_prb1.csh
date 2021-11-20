#!/bin/csh
#
F77 -c -g smdlib_prb1.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling smdlib_prb1.f"
  exit
endif
rm compiler.txt
#
F77 smdlib_prb1.o -L$HOME/libf77/$ARCH -lsmdlib
if ( $status != 0 ) then
  echo "Errors linking and loading smdlib_prb1.o"
  exit
endif
rm smdlib_prb1.o
#
mv a.out smdlib_prb1
./smdlib_prb1 > smdlib_prb1_output.txt
if ( $status != 0 ) then
  echo "Errors running smdlib_prb1"
  exit
endif
rm smdlib_prb1
#
echo "Test results written to smdlib_prb1_output.txt."
