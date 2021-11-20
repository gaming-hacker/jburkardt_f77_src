#!/bin/csh
#
F77 -c -g tidvt.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling tidvt.f"
  exit
endif
rm compiler.txt
#
F77 tidvt.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading tidvt.o"
  exit
endif
rm tidvt.o
#
mv a.out tidvt
./tidvt > tidvt_output.txt
if ( $status != 0 ) then
  echo "Errors running tidvt"
  exit
endif
rm tidvt
#
echo "Test results written to tidvt_output.txt."
