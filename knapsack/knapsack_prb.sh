#!/bin/bash
#
gfortran -c knapsack_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling knapsack_prb.f"
  exit
fi
#
gfortran knapsack_prb.o -L$HOME/libf77 -lknapsack
if [ $? -ne 0 ]; then
  echo "Errors linking and loading knapsack_prb.o"
  exit
fi
rm knapsack_prb.o
#
mv a.out knapsack_prb
./knapsack_prb > knapsack_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running knapsack_prb"
  exit
fi
rm knapsack_prb
#
echo "Test results written to knapsack_prb_output.txt."
