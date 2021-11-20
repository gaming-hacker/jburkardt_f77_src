#!/bin/bash
#
gfortran -c specfun_prb1.f
if [ $? -ne 0 ]; then
  echo "Errors compiling specfun_prb1.f"
  exit
fi
#
gfortran specfun_prb1.o -L$HOME/libf77 -lspecfun
if [ $? -ne 0 ]; then
  echo "Errors linking and loading specfun_prb1.o"
  exit
fi
rm specfun_prb1.o
#
mv a.out specfun_prb1
./specfun_prb1 > specfun_prb1_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running specfun_prb1"
  exit
fi
rm specfun_prb1
#
echo "Test results written to specfun_prb1_output.txt."
