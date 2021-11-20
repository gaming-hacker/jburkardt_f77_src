#!/bin/bash
#
gfortran -c asa243_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa243_prb.f"
  exit
fi
#
gfortran asa243_prb.o -L$HOME/libf77 -lasa243
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa243_prb.o"
  exit
fi
rm asa243_prb.o
#
mv a.out asa243_prb
./asa243_prb > asa243_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa243_prb"
  exit
fi
rm asa243_prb
#
echo "Test results written to asa243_prb_output.txt."
