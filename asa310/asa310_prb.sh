#!/bin/bash
#
gfortran -c asa310_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa310_prb.f"
  exit
fi
#
gfortran asa310_prb.o -L$HOME/libf77 -lasa310
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa310_prb.o"
  exit
fi
rm asa310_prb.o
#
mv a.out asa310_prb
./asa310_prb > asa310_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa310_prb"
  exit
fi
rm asa310_prb
#
echo "Test results written to asa310_prb_output.txt."
