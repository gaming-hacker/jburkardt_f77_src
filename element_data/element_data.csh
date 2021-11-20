#!/bin/csh
#
F77 -c element_data.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling element_data.f"
  exit
endif
rm compiler.txt
#
F77 element_data.o
if ( $status != 0 ) then
  echo "Errors loading element_data.o"
  exit
endif
rm element_data.o
#
mv a.out ~/binf77/$ARCH/element_data
#
echo "Executable installed as ~/binf77/$ARCH/element_data"
