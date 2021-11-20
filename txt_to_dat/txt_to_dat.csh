#!/bin/csh
#
F77 -c -g txt_to_dat.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling txt_to_dat.f"
  exit
endif
rm compiler.txt
#
F77 txt_to_dat.o
if ( $status != 0 ) then
  echo "Errors linking and loading txt_to_dat.o"
  exit
endif
rm txt_to_dat.o
#
chmod ugo+x a.out
mv a.out ~/binf77/$ARCH/txt_to_dat
#
echo "Executable installed as ~/binf77/$ARCH/txt_to_dat"
