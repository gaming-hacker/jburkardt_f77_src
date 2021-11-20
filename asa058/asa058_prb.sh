#!/bin/bash
#
gfortran -c asa058_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa058_prb.f"
  exit
fi
#
gfortran asa058_prb.o -L$HOME/libf77 -lasa058
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa058_prb.o"
  exit
fi
rm asa058_prb.o
#
mv a.out asa058_prb
./asa058_prb > asa058_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa058_prb"
  exit
fi
rm asa058_prb
#
echo "Test results written to asa058_prb_output.txt."
