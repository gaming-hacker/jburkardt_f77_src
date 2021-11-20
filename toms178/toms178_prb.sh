#!/bin/bash
#
gfortran -c toms178_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms178_prb.f"
  exit
fi
#
gfortran toms178_prb.o -L$HOME/libf77 -ltoms178
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms178_prb.o"
  exit
fi
rm toms178_prb.o
#
mv a.out toms178_prb
./toms178_prb > toms178_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms178_prb"
  exit
fi
rm toms178_prb
#
echo "Test results written to toms178_prb_output.txt."
