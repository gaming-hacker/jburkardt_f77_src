#!/bin/csh
#
F77 -c -g calcomp_prb.f >& compiler.tx
if ( $status != 0 ) then
  echo "Errors compiling calcomp_prb.f"
  exit
endif
rm compiler.txt
#
F77 calcomp_prb.o -L$HOME/libf77/$ARCH -lcalcomp
if ( $status != 0 ) then
  echo "Errors linking and loading calcomp_prb.o"
  exit
endif
rm calcomp_prb.o
#
mv a.out calcomp_prb
./calcomp_prb > calcomp_prb_output.txt
if ( $status != 0 ) then
  echo "Errors running calcomp_prb"
  exit
endif
rm calcomp_prb
#
echo "Program output written to calcomp_prb_output.txt"
