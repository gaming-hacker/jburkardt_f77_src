#!/bin/bash
#
gfortran -c rkf45_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling rkf45_prb.f"
  exit
fi
#
gfortran rkf45_prb.o -L$HOME/libf77 -lrkf45
if [ $? -ne 0 ]; then
  echo "Errors linking and loading rkf45_prb.o"
  exit
fi
rm rkf45_prb.o
#
mv a.out rkf45_prb
./rkf45_prb > rkf45_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running rkf45_prb"
  exit
fi
rm rkf45_prb
#
echo "Test results written to rkf45_prb_output.txt."
