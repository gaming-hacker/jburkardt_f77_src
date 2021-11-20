#!/bin/csh
#
F77 -c -g advec.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling advec.f"
  exit
endif
rm compiler.txt
#
F77 advec.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading advec.o"
  exit
endif
rm advec.o
#
mv a.out advec
./advec > advec_output.txt
if ( $status != 0 ) then
  echo "Errors running advec"
  exit
endif
rm advec
#
echo "Test results written to advec_output.txt."
