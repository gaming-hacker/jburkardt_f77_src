#!/bin/csh
#
F77 -c -g newnon.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling newnon.f"
  exit
endif
rm compiler.txt
#
F77 newnon.o -L$HOME/libf77/$ARCH -ltest_nonlin
if ( $status != 0 ) then
  echo "Errors linking and loading newnon.o"
  exit
endif
rm newnon.o
#
chmod ugo+x a.out
mv a.out ~/binf77/$ARCH/newnon
#
echo "Executable installed as ~/binf77/$ARCH/newnon"
