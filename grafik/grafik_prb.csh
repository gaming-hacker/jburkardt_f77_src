#!/bin/csh
#
F77 -c -g grafik_prb.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling grafik_prb.f"
  exit
endif
rm compiler.txt
#
F77 grafik_prb.o -L$HOME/libf77/$ARCH -lgrafik -lanybug
if ( $status != 0 ) then
  echo "Errors linking and loading grafik_prb.o"
  exit
endif
rm grafik_prb.o
#
mv a.out grafik_prb
./grafik_prb > grafik_prb_output.txt
if ( $status != 0 ) then
  echo "Errors running grafik_prb"
  exit
endif
rm grafik_prb
#
echo "Test results written to grafik_prb_output.txt."
