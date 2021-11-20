#!/bin/bash
#
gfortran -c random_data_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling random_data_prb.f"
  exit
fi
#
gfortran random_data_prb.o -L$HOME/libf77 -lrandom_data
if [ $? -ne 0 ]; then
  echo "Errors linking and loading random_data_prb.o"
  exit
fi
rm random_data_prb.o
#
mv a.out random_data_prb
./random_data_prb > random_data_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running random_data_prb"
  exit
fi
rm random_data_prb
#
echo "Program output written to random_data_prb_output.txt"
