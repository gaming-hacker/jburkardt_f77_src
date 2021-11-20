#!/bin/csh
#
F77 -c -g tshpg.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling tshpg.f"
  exit
endif
rm compiler.txt
#
F77 tshpg.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading tshpg.o"
  exit
endif
rm tshpg.o
#
mv a.out tshpg
./tshpg > tshpg_output.txt
if ( $status != 0 ) then
  echo "Errors running tshpg"
  exit
endif
rm tshpg
#
echo "Test results written to tshpg_output.txt."
