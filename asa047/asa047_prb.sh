#!/bin/bash
#
gfortran -c asa047_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa047_prb.f"
  exit
fi
#
gfortran asa047_prb.o -L$HOME/libf77 -lasa047
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa047_prb.o"
  exit
fi
rm asa047_prb.o
#
mv a.out asa047_prb
./asa047_prb > asa047_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa047_prb"
  exit
fi
rm asa047_prb
#
echo "Test results written to asa047_prb_output.txt."
