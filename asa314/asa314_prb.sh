#!/bin/bash
#
gfortran -c asa314_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa314_prb.f"
  exit
fi
#
gfortran asa314_prb.o -L$HOME/libf77 -lasa314
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa314_prb.o"
  exit
fi
rm asa314_prb.o
#
mv a.out asa314_prb
./asa314_prb > asa314_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa314_prb"
  exit
fi
rm asa314_prb
#
echo "Test results written to asa314_prb_output.txt."
