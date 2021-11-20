#!/bin/csh
#
F77 -c compare.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling compare.f"
  exit
endif
rm compiler.txt
#
F77 compare.o -L$HOME/libf77/$ARCH -ltoms611 -lmachine
if ( $status != 0 ) then
  echo "Errors linking and loading compare.o"
  exit
endif
rm compare.o
#
chmod ugo+x a.out
mv a.out ~/binf77/$ARCH/compare
#
echo "Program installed as ~/binf77/$ARCH/compare"
