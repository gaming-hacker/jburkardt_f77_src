#!/bin/csh
#
F77 -c arby1.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling arby1.f"
  exit
endif
rm compiler.txt
#
F77 arby1.o -L$HOME/libf77/$ARCH -ltoms611 -lmachine
if ( $status != 0 ) then
  echo "Errors linking and loading arby1.o"
  exit
endif
rm arby1.o
#
chmod ugo+x a.out
mv a.out ~/binf77/$ARCH/arby1
#
echo "Program installed as ~/binf77/$ARCH/arby1"
