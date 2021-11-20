#!/bin/bash
#
gfortran -c asa172_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa172_prb.f"
  exit
fi
#
gfortran asa172_prb.o -L$HOME/libf77 -lasa172
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa172_prb.o"
  exit
fi
rm asa172_prb.o
#
mv a.out asa172_prb
./asa172_prb > asa172_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa172_prb"
  exit
fi
rm asa172_prb
#
echo "Test results written to asa172_prb_output.txt."
