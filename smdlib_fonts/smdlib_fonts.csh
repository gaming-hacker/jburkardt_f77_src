#!/bin/csh
#
F77 -c -g smdlib_fonts.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling smdlib_fonts.f"
  exit
endif
rm compiler.txt
#
F77 smdlib_fonts.o
if ( $status != 0 ) then
  echo "Errors linking and loading smdlib_fonts.o"
  exit
endif
rm smdlib_fonts.o
#
chmod ugo+x a.out
mv a.out ~/binf77/$ARCH/smdlib_fonts
#
echo "Program installed as ~/binf77/$ARCH/smdlib_fonts"
