#!/bin/csh
#
set echo
#
g77 -c -g example04.f >& compiler.out
if ( $status != 0 ) then
  echo "Errors compiling example04.f"
  exit
endif
rm compiler.out
#
g77 example04.o -L$HOME/libf77/$ARCH -lclawpack
if ( $status != 0 ) then
  echo "Errors linking and loading example04.o"
  exit
endif
rm example04.o
#
mv a.out example04
./example04 > example04.out
if ( $status != 0 ) then
  echo "Errors running example04"
  exit
endif
rm example04
#
echo "example04 has been executed."
