#!/bin/csh
#
F77 -c -g zero_prb.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling zero_prb.f"
  exit
endif
rm compiler.txt
#
F77 zero_prb.o -L$HOME/libf77/$ARCH -lzero -lmachine
if ( $status != 0 ) then
  echo "Errors linking and loading zero_prb.o"
  exit
endif
rm zero_prb.o
#
mv a.out zero_prb
./zero_prb > zero_prb_output.txt
if ( $status != 0 ) then
  echo "Errors running zero_prb"
  exit
endif
rm zero_prb
#
echo "Program output written to zero_prb_output.txt"
