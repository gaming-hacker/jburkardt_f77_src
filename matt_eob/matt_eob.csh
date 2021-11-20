#!/bin/csh
#
F77 -c -g matt_eob.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling matt_eob.f"
  exit
endif
rm compiler.txt
#
F77 matt_eob.o
if ( $status != 0 ) then
  echo "Errors linking and loading matt_eob.o"
  exit
endif
rm matt_eob.o
#
chmod ugo+x a.out
mv a.out ~/binf77/$ARCH/matt_eob
#
echo "Executable installed as ~/binf77/$ARCH/matt_eob"
