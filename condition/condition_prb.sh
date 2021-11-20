#!/bin/bash
#
gfortran -c condition_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling condition_prb.f"
  exit
fi
#
gfortran condition_prb.o -L$HOME/libf77 -lcondition -lr8lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading condition_prb.o"
  exit
fi
rm condition_prb.o
#
mv a.out condition_prb
./condition_prb > condition_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running condition_prb"
  exit
fi
rm condition_prb
#
echo "Program output written to condition_prb_output.txt"
