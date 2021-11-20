#!/bin/csh
#
F77 -c -g shallow.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling shallow.f"
  exit
endif
rm compiler.txt
#
F77 shallow.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading shallow.o"
  exit
endif
rm shallow.o
#
mv a.out shallow
./shallow > shallow_output.txt
if ( $status != 0 ) then
  echo "Errors running shallow"
  exit
endif
rm shallow
#
echo "Test results written to shallow_output.txt."
