#! /bin/bash
#
gfortran -c -Wall blas1_d.f
#
ar r ~/libf77/libblas.a *.o
rm *.o
#
echo "Normal end of execution."
