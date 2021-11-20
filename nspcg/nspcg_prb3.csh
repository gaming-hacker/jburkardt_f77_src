#!/bin/csh
#
F77 -c -g nspcg_prb3.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling nspcg_prb3.f"
  exit
endif
rm compiler.txt
#
F77 nspcg_prb3.o -L$HOME/libf77/$ARCH -lnspcg
if ( $status != 0 ) then
  echo "Errors linking and loading nspcg_prb3.o"
  exit
endif
rm nspcg_prb3.o
#
mv a.out nspcg_prb3
./nspcg_prb3 > nspcg_prb3_output.txt
if ( $status != 0 ) then
  echo "Errors running nspcg_prb3"
  exit
endif
rm nspcg_prb3
#
echo "Test results written to nspcg_prb3_output.txt."
