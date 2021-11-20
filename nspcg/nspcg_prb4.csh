#!/bin/csh
#
F77 -c -g nspcg_prb4.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling nspcg_prb4.f"
  exit
endif
rm compiler.txt
#
F77 nspcg_prb4.o -L$HOME/libf77/$ARCH -lnspcg
if ( $status != 0 ) then
  echo "Errors linking and loading nspcg_prb4.o"
  exit
endif
rm nspcg_prb4.o
#
mv a.out nspcg_prb4
./nspcg_prb4 > nspcg_prb4_output.txt
if ( $status != 0 ) then
  echo "Errors running nspcg_prb4"
  exit
endif
rm nspcg_prb4
#
echo "Test results written to nspcg_prb4_output.txt."
