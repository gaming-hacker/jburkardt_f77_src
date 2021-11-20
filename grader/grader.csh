#!/bin/csh
#
F77 -c -g grader.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling grader.f"
  exit
endif
rm compiler.txt
#
F77 grader.o
if ( $status != 0 ) then
  echo "Errors linking and loading grader.o"
  exit
endif
rm grader.o
#
chmod ugo+x a.out
mv a.out ~/binf77/$ARCH/grader
#
echo "Executable installed as ~/binf77/$ARCH/grader"
