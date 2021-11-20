#!/bin/csh
#
F77 -c arby3.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling arby3.f"
  exit
endif
rm compiler.txt
#
F77 arby3.o -L$HOME/libf77/$ARCH -ltoms611 -lmachine
if ( $status != 0 ) then
  echo "Errors linking and loading arby3.o"
  exit
endif
rm arby3.o
#
chmod ugo+x a.out
mv a.out ~/binf77/$ARCH/arby3
#
echo "Program installed as ~/binf77/$ARCH/arby3"
