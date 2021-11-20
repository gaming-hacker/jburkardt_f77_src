#!/bin/csh
#
F77 -c -g tgrad.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling tgrad.f"
  exit
endif
rm compiler.txt
#
F77 tgrad.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading tgrad.o"
  exit
endif
rm tgrad.o
#
mv a.out tgrad
./tgrad > tgrad_output.txt
if ( $status != 0 ) then
  echo "Errors running tgrad"
  exit
endif
rm tgrad
#
echo "Test results written to tgrad_output.txt."
