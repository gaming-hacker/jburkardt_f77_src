#!/bin/bash
#
gfortran -c asa241_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa241_prb.f"
  exit
fi
#
gfortran asa241_prb.o -L$HOME/libf77 -lasa241
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa241_prb.o"
  exit
fi
rm asa241_prb.o
#
mv a.out asa241_prb
./asa241_prb > asa241_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa241_prb"
  exit
fi
rm asa241_prb
#
echo "Test results written to asa241_prb_output.txt."
