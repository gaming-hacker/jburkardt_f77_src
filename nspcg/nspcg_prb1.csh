#!/bin/csh
#
F77 -c -g nspcg_prb1.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling nspcg_prb1.f"
  exit
endif
rm compiler.txt
#
F77 nspcg_prb1.o -L$HOME/libf77/$ARCH -lnspcg
if ( $status != 0 ) then
  echo "Errors linking and loading nspcg_prb1.o"
  exit
endif
rm nspcg_prb1.o
#
mv a.out nspcg_prb1
./nspcg_prb1 > nspcg_prb1_output.txt
if ( $status != 0 ) then
  echo "Errors running nspcg_prb1"
  exit
endif
rm nspcg_prb1
#
echo "Test results written to nspcg_prb1_output.txt."
