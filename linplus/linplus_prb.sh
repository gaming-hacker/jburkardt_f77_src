#!/bin/bash
#
gfortran -c linplus_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling linplus_prb.f"
  exit
fi
#
gfortran linplus_prb.o -L$HOME/libf77 -llinplus
if [ $? -ne 0 ]; then
  echo "Errors linking and loading linplus_prb.o"
  exit
fi
rm linplus_prb.o
#
mv a.out linplus_prb
./linplus_prb > linplus_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running linplus_prb"
  exit
fi
rm linplus_prb
#
echo "Test results written to linplus_prb_output.txt."
