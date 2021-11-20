#!/bin/csh
#  ranprb.csh  20 February 1999
#
setenv DEBUG FALSE
set echo
#
if ( $DEBUG == TRUE ) then
  f77 -c -C -static -trapuv -u -w0 ranprb.f
  if ( $status != 0 ) exit
else
  f77 -c ranprb.f
  if ( $status != 0 ) exit
endif
#
f77 ranprb.o ~/lib/$ARCH/libranpack.a
if ( $status != 0 ) exit
rm ranprb.o
#
mv a.out ranprb
ranprb > ranprb.out
#
if ($status == 0) then
  rm ranprb
endif
