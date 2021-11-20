#!/bin/bash
#
gfortran -c asa121_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa121_prb.f"
  exit
fi
#
gfortran asa121_prb.o -L$HOME/libf77 -lasa121
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa121_prb.o"
  exit
fi
rm asa121_prb.o
#
mv a.out asa121_prb
./asa121_prb > asa121_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa121_prb"
  exit
fi
rm asa121_prb
#
echo "Test results written to asa121_prb_output.txt."
