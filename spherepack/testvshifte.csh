#!/bin/csh
#
F77 -c -g testvshifte.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling testvshifte.f"
  exit
endif
rm compiler.txt
#
F77 testvshifte.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading testvshifte.o"
  exit
endif
rm testvshifte.o
#
mv a.out testvshifte
./testvshifte > testvshifte_output.txt
if ( $status != 0 ) then
  echo "Errors running testvshifte"
  exit
endif
rm testvshifte
#
echo "Test results written to testvshifte_output.txt."
