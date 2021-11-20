#!/bin/csh
#
F77 -c -g testvtsgs.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling testvtsgs.f"
  exit
endif
rm compiler.txt
#
F77 testvtsgs.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading testvtsgs.o"
  exit
endif
rm testvtsgs.o
#
mv a.out testvtsgs
./testvtsgs > testvtsgs_output.txt
if ( $status != 0 ) then
  echo "Errors running testvtsgs"
  exit
endif
rm testvtsgs
#
echo "Test results written to testvtsgs_output.txt."
