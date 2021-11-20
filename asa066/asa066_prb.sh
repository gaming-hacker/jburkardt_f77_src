#!/bin/bash
#
gfortran -c asa066_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa066_prb.f"
  exit
fi
#
gfortran asa066_prb.o -L$HOME/libf77 -lasa066
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa066_prb.o"
  exit
fi
rm asa066_prb.o
#
mv a.out asa066_prb
./asa066_prb > asa066_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa066_prb"
  exit
fi
rm asa066_prb
#
echo "Test results written to asa066_prb_output.txt."
