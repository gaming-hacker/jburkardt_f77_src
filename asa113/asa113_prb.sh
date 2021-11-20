#!/bin/bash
#
gfortran -c asa113_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa113_prb.f"
  exit
fi
#
gfortran asa113_prb.o -L$HOME/libf77 -lasa113
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa113_prb.o"
  exit
fi
rm asa113_prb.o
#
mv a.out asa113_prb
./asa113_prb > asa113_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa113_prb"
  exit
fi
rm asa113_prb
#
echo "Test results written to asa113_prb_output.txt."
