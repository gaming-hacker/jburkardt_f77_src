#!/bin/csh
#
F77 -c -g tdiv.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling tdiv.f"
  exit
endif
rm compiler.txt
#
F77 tdiv.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading tdiv.o"
  exit
endif
rm tdiv.o
#
mv a.out tdiv
./tdiv > tdiv_output.txt
if ( $status != 0 ) then
  echo "Errors running tdiv"
  exit
endif
rm tdiv
#
echo "Test results written to tdiv_output.txt."
