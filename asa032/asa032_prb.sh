#!/bin/bash
#
gfortran -c asa032_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa032_prb.f"
  exit
fi
#
gfortran asa032_prb.o -L$HOME/libf77 -lasa032
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa032_prb.o"
  exit
fi
rm asa032_prb.o
#
mv a.out asa032_prb
./asa032_prb > asa032_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa032_prb"
  exit
fi
rm asa032_prb
#
echo "Test results written to asa032_prb_output.txt."
