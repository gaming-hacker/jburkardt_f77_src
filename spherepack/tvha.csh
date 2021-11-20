#!/bin/csh
#
F77 -c -g tvha.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling tvha.f"
  exit
endif
rm compiler.txt
#
F77 tvha.o -L$HOME/libf77/$ARCH -lspherepack
if ( $status != 0 ) then
  echo "Errors linking and loading tvha.o"
  exit
endif
rm tvha.o
#
mv a.out tvha
./tvha > tvha_output.txt
if ( $status != 0 ) then
  echo "Errors running tvha"
  exit
endif
rm tvha
#
echo "Test results written to tvha_output.txt."
