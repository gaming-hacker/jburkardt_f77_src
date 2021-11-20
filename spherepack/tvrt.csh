#!/bin/csh
#
F77 -c -g tvrt.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling tvrt.f"
  exit
endif
rm compiler.txt
#
F77 tvrt.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading tvrt.o"
  exit
endif
rm tvrt.o
#
mv a.out tvrt
./tvrt > tvrt_output.txt
if ( $status != 0 ) then
  echo "Errors running tvrt"
  exit
endif
rm tvrt
#
echo "Test results written to tvrt_output.txt."
