#!/bin/bash
#
gfortran -c asa144_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa144_prb.f"
  exit
fi
#
gfortran asa144_prb.o -L$HOME/libf77 -lasa144
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa144_prb.o"
  exit
fi
rm asa144_prb.o
#
mv a.out asa144_prb
./asa144_prb > asa144_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa144_prb"
  exit
fi
rm asa144_prb
#
echo "Test results written to asa144_prb_output.txt."
