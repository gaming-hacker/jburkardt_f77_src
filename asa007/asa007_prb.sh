#!/bin/bash
#
gfortran -c asa007_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa007_prb.f"
  exit
fi
#
gfortran asa007_prb.o -L$HOME/libf77 -lasa007
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa007_prb.o"
  exit
fi
rm asa007_prb.o
#
mv a.out asa007_prb
./asa007_prb > asa007_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa007_prb"
  exit
fi
rm asa007_prb
#
echo "Test results written to asa007_prb_output.txt."
