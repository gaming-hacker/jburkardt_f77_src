#!/bin/csh
#
F77 -c -g pfort.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling pfort.f"
  exit
endif
rm compiler.txt
#
F77 pfort.o
if ( $status != 0 ) then
  echo "Errors linking and loading pfort.o"
  exit
endif
rm pfort.o
#
chmod ugo+x a.out
mv a.out ~/binf77/$ARCH/pfort
#
echo "Executable installed as ~/binf77/$ARCH/pfort"
