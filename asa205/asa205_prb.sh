#!/bin/bash
#
gfortran -c asa205_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa205_prb.f"
  exit
fi
#
gfortran asa205_prb.o -L$HOME/libf77 -lasa205
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa205_prb.o"
  exit
fi
rm asa205_prb.o
#
mv a.out asa205_prb
./asa205_prb > asa205_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa205_prb"
  exit
fi
rm asa205_prb
#
echo "Test results written to asa205_prb_output.txt."
