#!/bin/bash
#
gfortran -c asa076_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling asa076_prb.f"
  exit
fi
#
gfortran asa076_prb.o -L$HOME/libf77 -lasa076
if [ $? -ne 0 ]; then
  echo "Errors linking and loading asa076_prb.o"
  exit
fi
rm asa076_prb.o
#
mv a.out asa076_prb
./asa076_prb > asa076_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running asa076_prb"
  exit
fi
rm asa076_prb
#
echo "Test results written to asa076_prb_output.txt."
