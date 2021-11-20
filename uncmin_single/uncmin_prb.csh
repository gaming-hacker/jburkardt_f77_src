#!/bin/csh
#
F77 -c -g uncmin_prb.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling uncmin_prb.f"
  exit
endif
rm compiler.txt
#
F77 uncmin_prb.o -L$HOME/libf77/$ARCH -luncmin
if ( $status != 0 ) then
  echo "Errors linking and loading uncmin_prb.o"
  exit
endif
rm uncmin_prb.o
#
mv a.out uncmin_prb
./uncmin_prb > uncmin_prb_output.txt
if ( $status != 0 ) then
  echo "Errors running uncmin_prb"
  exit
endif
rm uncmin_prb
#
echo "Test results written to uncmin_prb_output.txt."
