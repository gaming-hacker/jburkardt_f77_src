#!/bin/csh
#
F77 -c -g rk124.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling rk124.f"
  exit
endif
rm compiler.txt
#
F77 rk124.o -L$HOME/libf77/$ARCH -lanytty
if ( $status != 0 ) then
  echo "Errors linking and loading rk124.o"
  exit
endif
rm rk124.o
#
chmod ugo+x a.out
mv a.out ~/binf77/$ARCH/rk124
#
echo "Program installed as ~/binf77/$ARCH/rk124"
