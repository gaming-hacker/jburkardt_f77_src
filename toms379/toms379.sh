#! /bin/bash
#
gfortran -Wall -c toms379.f
#
ar qc libtoms379.a *.o
rm *.o
#
mv libtoms379.a ~/libf77
#
echo "Normal end of execution."
