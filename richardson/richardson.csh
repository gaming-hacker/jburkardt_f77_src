#!/bin/csh
#
F77 -c -g richardson.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling richardson.f"
  exit
endif
rm compiler.txt
#
F77 richardson.o
if ( $status != 0 ) then
  echo "Errors linking and loading richardson.o"
  exit
endif
rm richardson.o
#
chmod ugo+x a.out
mv a.out ~/binf77/$ARCH/richardson
#
echo "Executable installed as ~/binf77/$ARCH/richardson"
