#!/bin/csh
#
F77 -c -g matt_message.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling matt_message.f"
  exit
endif
rm compiler.txt
#
F77 matt_message.o
if ( $status != 0 ) then
  echo "Errors linking and loading matt_message.o"
  exit
endif
rm matt_message.o
#
chmod ugo+x a.out
mv a.out ~/binf77/$ARCH/matt_message
#
echo "Executable installed as ~/binf77/$ARCH/matt_message"
