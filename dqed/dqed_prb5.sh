#!/bin/bash
#
gfortran -c dqed_prb5.f
if [ $? -ne 0 ]; then
  echo "Errors compiling dqed_prb5.f"
  exit
fi
#
gfortran dqed_prb5.o -L$HOME/libf77 -ldqed
if [ $? -ne 0 ]; then
  echo "Errors linking and loading dqed_prb5.o"
  exit
fi
rm dqed_prb5.o
#
mv a.out dqed_prb5
./dqed_prb5 > dqed_prb5_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running dqed_prb5"
  exit
fi
rm dqed_prb5
#
echo "Test results written to dqed_prb5_output.txt."
