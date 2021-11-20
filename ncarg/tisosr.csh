#!/bin/csh
#
set echo
#
setenv NCARG_ROOT /usr/local/ncarg
#
f77 -c tisosr.f >& compiler.out
if ( $status != 0 ) then
  echo "Errors compiling tisosr.f"
  exit
endif
rm compiler.out
#
f77 tisosr.o -L$NCARG_ROOT/lib -L/usr/X11R6/lib -lncarg -lncarg_gks -lncarg_c -lX11
if ( $status != 0 ) then
  echo "Errors linking and loading tisosr.o"
  exit
endif
rm tisosr.o
#
mv a.out tisosr
./tisosr > tisosr.out
if ( $status != 0 ) then
  echo "Errors running tisosr"
  exit
endif
rm tisosr
mv gmeta1.ps tisosr.eps
#
echo "The tisosr program has been executed."
