#!/bin/csh
#
F77 -c -g nspcg_prb2.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling nspcg_prb2.f"
  exit
endif
rm compiler.txt
#
F77 nspcg_prb2.o -L$HOME/libf77/$ARCH -lnspcg
if ( $status != 0 ) then
  echo "Errors linking and loading nspcg_prb2.o"
  exit
endif
rm nspcg_prb2.o
#
mv a.out nspcg_prb2
./nspcg_prb2 > nspcg_prb2_output.txt
if ( $status != 0 ) then
  echo "Errors running nspcg_prb2"
  exit
endif
rm nspcg_prb2
#
echo "Test results written to nspcg_prb2_output.txt."
