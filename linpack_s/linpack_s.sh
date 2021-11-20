#! /bin/bash
#
gfortran -c -Wall linpack_s.f
#
ar qc liblinpack_s.a *.o
rm *.o
#
mv liblinpack_s.a ~/libf77
#
echo "Normal end of execution."
