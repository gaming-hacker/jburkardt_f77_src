#!/bin/csh
#
F77 -c -g testrvsph.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling testrvsph.f"
  exit
endif
rm compiler.txt
#
F77 testrvsph.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading testrvsph.o"
  exit
endif
rm testrvsph.o
#
mv a.out testrvsph
./testrvsph > testrvsph_output.txt
if ( $status != 0 ) then
  echo "Errors running testrvsph"
  exit
endif
rm testrvsph
#
echo "Test results written to testrvsph_output.txt."
