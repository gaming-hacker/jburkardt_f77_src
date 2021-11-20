#!/bin/csh
#
F77 -c -g nspcg_fem_solve.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling nspcg_fem_solve.f"
  exit
endif
rm compiler.txt
#
F77 nspcg_fem_solve.o -L$HOME/libf77/$ARCH -lnspcg
if ( $status != 0 ) then
  echo "Errors linking and loading nspcg_fem_solve.o"
  exit
endif
rm nspcg_fem_solve.o
#
chmod ugo+x a.out
mv a.out ~/binf77/$ARCH/nspcg_fem_solve
#
echo "Executable installed as ~/binf77/$ARCH/nspcg_fem_solve"
