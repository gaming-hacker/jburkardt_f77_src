#!/bin/csh
#
F77 -c -g newton_prb2.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling newton_prb2.f"
  exit
endif
rm compiler.txt
#
F77 newton_prb2.o -L$HOME/libf77/$ARCH -lnewton
if ( $status != 0 ) then
  echo "Errors linking and loading newton_prb2.o"
  exit
endif
rm newton_prb2.o
#
mv a.out newton_prb2
#
echo "Interactive executable program created as newton_prb2"
