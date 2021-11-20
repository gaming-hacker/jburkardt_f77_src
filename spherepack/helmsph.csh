#!/bin/csh
#
F77 -c -g helmsph.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling helmsph.f"
  exit
endif
rm compiler.txt
#
F77 helmsph.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading helmsph.o"
  exit
endif
rm helmsph.o
#
mv a.out helmsph
./helmsph > helmsph_output.txt
if ( $status != 0 ) then
  echo "Errors running helmsph"
  exit
endif
rm helmsph
#
echo "Test results written to helmsph_output.txt."
