#!/bin/csh
#
set echo
#
f77 smdprb6.f ~/lib/libsmdlib.a ~/lib/libdrawcgmx.a -lgl_s -lXt -lX11 -lXmu -lXirisw
rm smdprb6.o
mv a.out smdprb6
smdprb6
#
rm smdprb6
