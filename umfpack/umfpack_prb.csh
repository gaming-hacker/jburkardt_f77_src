#!/bin/csh
#
F77 -c -g umfpack_prb.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling umfpack_prb.f"
  exit
endif
rm compiler.txt
#
F77 umfpack_prb.o -L$HOME/libf77/$ARCH -lumfpack
if ( $status != 0 ) then
  echo "Errors linking and loading umfpack_prb.o"
  exit
endif
rm umfpack_prb.o
#
mv a.out umfpack_prb
umfpack_prb < umfpack_prb_input.txt > umfpack_prb_output.txt
rm umfpack_prb
#
echo "Program output written to umfpack_prb_output.txt"

