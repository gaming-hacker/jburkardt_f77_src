#!/bin/bash
#
gfortran -c asa266_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa266_prb.f"
  exit
fi
#
gfortran asa266_prb.o -L$HOME/libf77 -lasa266
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa266_prb.o"
  exit
fi
rm asa266_prb.o
#
mv a.out asa266_prb
./asa266_prb > asa266_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa266_prb"
  exit
fi
rm asa266_prb
#
echo "Test results written to asa266_prb_output.txt."
