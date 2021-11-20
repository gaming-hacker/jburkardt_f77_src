#!/bin/bash
#
gfortran -c asa111_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa111_prb.f"
  exit
fi
#
gfortran asa111_prb.o -L$HOME/libf77 -lasa111
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa111_prb.o"
  exit
fi
rm asa111_prb.o
#
mv a.out asa111_prb
./asa111_prb > asa111_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa111_prb"
  exit
fi
rm asa111_prb
#
echo "Test results written to asa111_prb_output.txt."
