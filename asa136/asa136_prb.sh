#!/bin/bash
#
gfortran -c asa136_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa136_prb.f"
  exit
fi
#
gfortran asa136_prb.o -L$HOME/libf77 -lasa136
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa136_prb.o"
  exit
fi
rm asa136_prb.o
#
mv a.out asa136_prb
./asa136_prb > asa136_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa136_prb"
  exit
fi
rm asa136_prb
#
echo "Test results written to asa136_prb_output.txt."
