#!/bin/bash
#
gfortran -c knapsack_01_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling knapsack_01_prb.f"
  exit
fi
#
gfortran -o knapsack_01_prb knapsack_01_prb.o -L$HOME/libf77 -lknapsack_01
if [ $? -ne 0 ]; then
  echo "Errors linking and loading knapsack_01_prb.o"
  exit
fi
rm knapsack_01_prb.o
#
./knapsack_01_prb > knapsack_01_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running knapsack_01_prb"
  exit
fi
rm knapsack_01_prb
#
echo "Test program output written to knapsack_01_prb_output.txt."
