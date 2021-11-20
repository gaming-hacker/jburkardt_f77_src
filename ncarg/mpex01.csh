#!/bin/csh
#
set echo
#
setenv NCARG_ROOT /usr/local/ncarg
#
f77 -c mpex01.f >& compiler.out
if ( $status != 0 ) then
  echo "Errors compiling mpex01.f"
  exit
endif
rm compiler.out
#
f77 mpex01.o -L$NCARG_ROOT/lib -L/usr/X11R6/lib -lncarg -lncarg_gks -lncarg_c -lX11
if ( $status != 0 ) then
  echo "Errors linking and loading mpex01.o"
  exit
endif
rm mpex01.o
#
mv a.out mpex01
./mpex01 > mpex01.out
if ( $status != 0 ) then
  echo "Errors running mpex01"
  exit
endif
rm mpex01
mv gmeta1.eps mpex01.eps
#
echo "The mpex01 program has been executed."
