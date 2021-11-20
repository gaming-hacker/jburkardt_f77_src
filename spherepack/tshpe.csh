#!/bin/csh
#
F77 -c -g tshpe.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling tshpe.f"
  exit
endif
rm compiler.txt
#
F77 tshpe.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading tshpe.o"
  exit
endif
rm tshpe.o
#
mv a.out tshpe
./tshpe > tshpe_output.txt
if ( $status != 0 ) then
  echo "Errors running tshpe"
  exit
endif
rm tshpe
#
echo "Test results written to tshpe_output.txt."
