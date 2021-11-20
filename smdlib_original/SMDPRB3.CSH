#!/bin/csh
#
set echo
#
f77 smdprb3.f ~/lib/libsmdlib.a ~/lib/libdrawcgmx.a -lgl_s -lXt -lX11 -lXmu -lXirisw
rm smdprb3.o
mv a.out smdprb3
smdprb3
#
rm smdprb3
