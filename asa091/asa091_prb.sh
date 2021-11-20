#!/bin/bash
#
gfortran -c asa091_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa091_prb.f"
  exit
fi
#
gfortran asa091_prb.o -L$HOME/libf77 -lasa091
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa091_prb.o"
  exit
fi
rm asa091_prb.o
#
mv a.out asa091_prb
./asa091_prb > asa091_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa091_prb"
  exit
fi
rm asa091_prb
#
echo "Test results written to asa091_prb_output.txt."
