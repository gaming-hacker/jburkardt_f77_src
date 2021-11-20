#!/bin/bash
#
gfortran -c normal_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling normal_prb.f"
  exit
fi
#
gfortran normal_prb.o -L$HOME/libf77 -lnormal
if [ $? -ne 0 ]; then
  echo "Errors linking and loading normal_prb.o"
  exit
fi
rm normal_prb.o
#
mv a.out normal_prb
./normal_prb > normal_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running normal_prb"
  exit
fi
rm normal_prb
#
echo "Test results written to normal_prb_output.txt."
