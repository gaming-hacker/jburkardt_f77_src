#!/bin/csh
#
F77 -c -g tgaqd.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling tgaqd.f"
  exit
endif
rm compiler.txt
#
F77 tgaqd.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading tgaqd.o"
  exit
endif
rm tgaqd.o
#
mv a.out tgaqd
./tgaqd > tgaqd_output.txt
if ( $status != 0 ) then
  echo "Errors running tgaqd"
  exit
endif
rm tgaqd
#
echo "Test results written to tgaqd_output.txt."
