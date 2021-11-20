#!/bin/csh
#
set echo
#
g77 -c -g example01.f >& compiler.out
if ( $status != 0 ) then
  echo "Errors compiling example01.f"
  exit
endif
rm compiler.out
#
g77 example01.o -L$HOME/libf77/$ARCH -lclawpack
if ( $status != 0 ) then
  echo "Errors linking and loading example01.o"
  exit
endif
rm example01.o
#
mv a.out example01
./example01 > example01.out
if ( $status != 0 ) then
  echo "Errors running example01"
  exit
endif
rm example01
#
echo "example01 has been executed."
