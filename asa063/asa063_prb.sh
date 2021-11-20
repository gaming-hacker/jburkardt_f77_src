#!/bin/bash
#
gfortran -c asa063_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa063_prb.f"
  exit
fi
#
gfortran asa063_prb.o -L$HOME/libf77 -lasa063
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa063_prb.o"
  exit
fi
rm asa063_prb.o
#
mv a.out asa063_prb
./asa063_prb > asa063_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa063_prb"
  exit
fi
rm asa063_prb
#
echo "Test results written to asa063_prb_output.txt."
