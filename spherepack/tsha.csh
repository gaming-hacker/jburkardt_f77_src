#!/bin/csh
#
F77 -c -g tsha.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling tsha.f"
  exit
endif
rm compiler.txt
#
F77 tsha.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading tsha.o"
  exit
endif
rm tsha.o
#
mv a.out tsha
./tsha > tsha_output.txt
if ( $status != 0 ) then
  echo "Errors running tsha"
  exit
endif
rm tsha
#
echo "Test results written to tsha_output.txt."
