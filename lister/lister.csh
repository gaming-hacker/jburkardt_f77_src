#!/bin/csh
#
F77 -c -g lister.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling lister.f"
  exit
endif
rm compiler.txt
#
F77 lister.o
if ( $status != 0 ) then
  echo "Errors linking and loading lister.o"
  exit
endif
rm lister.o
#
chmod ugo+x a.out
mv a.out ~/binf77/$ARCH/lister
#
echo "Executable installed as ~/binf77/$ARCH/lister"
