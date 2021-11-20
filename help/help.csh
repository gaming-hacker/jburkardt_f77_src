#!/bin/csh
#
F77 -c -g help.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling help.f"
  exit
endif
rm compiler.txt
#
F77 help.o
if ( $status != 0 ) then
  echo "Errors linking and loading help.o"
  exit
endif
rm help.o
#
chmod ugo+x a.out
mv a.out ~/binf77/$ARCH/help
#
echo "Program installed as ~/binf77/$ARCH/help"
