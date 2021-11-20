#!/bin/bash
#
gfortran -c toms661_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms661_prb.f"
  exit
fi
#
gfortran toms661_prb.o -L$HOME/libf77 -ltoms661
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms661_prb.o"
  exit
fi
rm toms661_prb.o
#
mv a.out toms661_prb
./toms661_prb > toms661_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms661_prb"
  exit
fi
rm toms661_prb
#
echo "Program output written to toms661_prb_output.txt"
