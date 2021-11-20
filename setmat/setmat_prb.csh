#!/bin/csh
#
F77 -c -g setmat_prb.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling setmat_prb.f"
  exit
endif
rm compiler.txt
#
F77 setmat_prb.o -L$HOME/libf77/$ARCH -lsetmat
if ( $status != 0 ) then
  echo "Errors linking and loading setmat_prb.o"
  exit
endif
rm setmat_prb.o
#
mv a.out setmat_prb
./setmat_prb > setmat_prb_output.txt
if ( $status != 0 ) then
  echo "Errors running setmat_prb"
  exit
endif
rm setmat_prb
#
echo "Test results written to setmat_prb_output.txt."
