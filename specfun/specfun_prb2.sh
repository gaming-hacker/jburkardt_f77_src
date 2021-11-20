#!/bin/bash
#
gfortran -c specfun_prb2.f
if [ $? -ne 0 ]; then
  echo "Errors compiling specfun_prb2.f"
  exit
fi
#
gfortran specfun_prb2.o -L$HOME/libf77 -lspecfun
if [ $? -ne 0 ]; then
  echo "Errors linking and loading specfun_prb2.o"
  exit
fi
rm specfun_prb2.o
#
mv a.out specfun_prb2
./specfun_prb2 > specfun_prb2_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running specfun_prb2"
  exit
fi
rm specfun_prb2
#
echo "Test results written to specfun_prb2_output.txt."
