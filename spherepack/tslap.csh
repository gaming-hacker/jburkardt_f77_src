#!/bin/csh
#
F77 -c -g tslap.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling tslap.f"
  exit
endif
rm compiler.txt
#
F77 tslap.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading tslap.o"
  exit
endif
rm tslap.o
#
mv a.out tslap
./tslap > tslap_output.txt
if ( $status != 0 ) then
  echo "Errors running tslap"
  exit
endif
rm tslap
#
echo "Test results written to tslap_output.txt."
