#!/bin/csh
#
F77 -c drawcgm_utilities_prb.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling drawcgm_utilities_prb.f90"
  exit
endif
rm compiler.txt
#
F77 drawcgm_utilities_prb.o -L$HOME/libf77/$ARCH -ldrawcgm_utilities -L$HOME/libc/$ARCH -ldrawcgm -L/usr/X11R6/lib -lX11 -lXt -lXmu
#
rm drawcgm_utilities_prb.o
mv a.out drawcgm_utilities_prb
drawcgm_utilities_prb > drawcgm_utilities_prb_output.txt
#
rm drawcgm_utilities_prb
#
echo "Program output written to drawcgm_utilities_prb_output.txt"
