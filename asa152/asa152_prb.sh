#!/bin/bash
#
gfortran -c asa152_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa152_prb.f"
  exit
fi
#
gfortran asa152_prb.o -L$HOME/libf77 -lasa152
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa152_prb.o"
  exit
fi
rm asa152_prb.o
#
mv a.out asa152_prb
./asa152_prb > asa152_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa152_prb"
  exit
fi
rm asa152_prb
#
echo "Test results written to asa152_prb_output.txt."
