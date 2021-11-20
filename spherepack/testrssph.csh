#!/bin/csh
#
F77 -c -g testrssph.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling testrssph.f"
  exit
endif
rm compiler.txt
#
F77 testrssph.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading testrssph.o"
  exit
endif
rm testrssph.o
#
mv a.out testrssph
./testrssph > testrssph_output.txt
if ( $status != 0 ) then
  echo "Errors running testrssph"
  exit
endif
rm testrssph
#
echo "Test results written to testrssph_output.txt."
