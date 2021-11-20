#!/bin/bash
#
gfortran -c asa159_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa159_prb.f"
  exit
fi
#
gfortran asa159_prb.o -L$HOME/libf77 -lasa159
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa159_prb.o"
  exit
fi
rm asa159_prb.o
#
mv a.out asa159_prb
./asa159_prb > asa159_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa159_prb"
  exit
fi
rm asa159_prb
#
echo "Test results written to asa159_prb_output.txt."
