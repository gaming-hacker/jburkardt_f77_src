#!/bin/csh
#
F77 -c -g newton_prb1.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling newton_prb1.f"
  exit
endif
rm compiler.txt
#
F77 newton_prb1.o -L$HOME/libf77/$ARCH -lnewton
if ( $status != 0 ) then
  echo "Errors linking and loading newton_prb1.o"
  exit
endif
rm newton_prb1.o
#
mv a.out newton_prb1
#
echo "Interactive executable program created as newton_prb1"
