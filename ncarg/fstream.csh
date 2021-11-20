#!/bin/csh
#
setenv NCARG_ROOT /usr/local/ncarg
#
F77 -c fstream.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling fstream.f"
  exit
endif
rm compiler.txt
#
f77 fstream.o -L$NCARG_ROOT/lib -L/usr/X11R6/lib -lncarg -lncarg_gks -lncarg_c -lX11
if ( $status != 0 ) then
  echo "Errors linking and loading fstream.o"
  exit
endif
rm fstream.o
#
mv a.out fstream
./fstream > fstream_output.txt
if ( $status != 0 ) then
  echo "Errors running fstream"
  exit
endif
rm fstream
mv gmeta1.eps fstream.eps
#
echo "Program output written to fstream_output.txt"
