#!/bin/csh
#
F77 -c arby2.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling arby2.f"
  exit
endif
rm compiler.txt
#
F77 arby2.o -L$HOME/libf77/$ARCH -ltoms611 -lmachine
if ( $status != 0 ) then
  echo "Errors linking and loading arby2.o"
  exit
endif
rm arby2.o
#
chmod ugo+x a.out
mv a.out ~/binf77/$ARCH/arby2
#
echo "Program installed as ~/binf77/$ARCH/arby2"
