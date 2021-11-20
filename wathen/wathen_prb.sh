#!/bin/bash
#
gfortran -c wathen_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling wathen_prb.f"
  exit
fi
#
gfortran -o wathen_prb wathen_prb.o -L$HOME/libf77 -lwathen
if [ $? -ne 0 ]; then
  echo "Errors linking and loading wathen_prb.o"
  exit
fi
rm wathen_prb.o
#
./wathen_prb > wathen_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running wathen_prb"
  exit
fi
rm wathen_prb
#
echo "Test program output written to wathen_prb_output.txt."
