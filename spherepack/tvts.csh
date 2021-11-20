#!/bin/csh
#
F77 -c -g tvts.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling tvts.f"
  exit
endif
rm compiler.txt
#
F77 tvts.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading tvts.o"
  exit
endif
rm tvts.o
#
mv a.out tvts
./tvts > tvts_output.txt
if ( $status != 0 ) then
  echo "Errors running tvts"
  exit
endif
rm tvts
#
echo "Test results written to tvts_output.txt."
