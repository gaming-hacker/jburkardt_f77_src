#!/bin/csh
#
set echo
#
mkdir ~/src/smdlib/work
cd ~/src/smdlib/work
#
rm *
fsplit ~/src/smdlib/smdlib.f
fsplit ~/src/smdlib/smdlibsgi.f
#
f77 -c *.f
rm *.f
#
ar qc libsmdlib.a *.o
rm *.o
#
mv libsmdlib.a ~/lib
#
cd ~/src/smdlib
rmdir ~/src/smdlib/work
