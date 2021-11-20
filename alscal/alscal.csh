#!/bin/csh
#
F77 -c alscal.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling alscal.f"
  exit
endif
rm compiler.txt
#
F77 alscal.o
if ( $status != 0 ) then
  echo "Errors linking and loading alscal.o"
  exit
endif
rm alscal.o
#
chmod ugo+x a.out
mv a.out ~/binf77/$ARCH/alscal
#
echo "Program installed as ~/binf77/$ARCH/alscal"
