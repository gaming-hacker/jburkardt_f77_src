#!/bin/bash
#
gfortran -c change_making_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling change_making_prb.f"
  exit
fi
#
gfortran -o change_making_prb change_making_prb.o -L$HOME/libf77 -lchange_making
if [ $? -ne 0 ]; then
  echo "Errors linking and loading change_making_prb.o"
  exit
fi
rm change_making_prb.o
#
./change_making_prb > change_making_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running change_making_prb"
  exit
fi
rm change_making_prb
#
echo "Test program output written to change_making_prb_output.txt."
