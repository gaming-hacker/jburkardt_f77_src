#! /bin/bash
#
gfortran -c -Wall linpack_d.f
#
ar qc liblinpack_d.a *.o
rm *.o
#
mv liblinpack_d.a ~/libf77
#
echo "Library installed as ~/libf77/liblinpack_d.a."
