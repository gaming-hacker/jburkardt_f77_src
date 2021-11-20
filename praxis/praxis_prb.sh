#!/bin/bash
#
gfortran -c praxis_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling praxis_prb.f"
  exit
fi
#
gfortran praxis_prb.o -L$HOME/libf77 -lpraxis
if [ $? -ne 0 ]; then
  echo "Errors linking and loading praxis_prb.o"
  exit
fi
rm praxis_prb.o
#
mv a.out praxis_prb
./praxis_prb > praxis_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running praxis_prb"
  exit
fi
rm praxis_prb
#
echo "Test results written to praxis_prb_output.txt."
