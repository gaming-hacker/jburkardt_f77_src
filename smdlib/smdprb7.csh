#!/bin/csh
#
set echo
#
f77 smdprb7.f ~/lib/libsmdlib.a ~/lib/libdrawcgmx.a -lgl_s -lXt -lX11 -lXmu -lXirisw
rm smdprb7.o
mv a.out smdprb7
smdprb7
#
rm smdprb7
