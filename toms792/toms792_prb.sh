#!/bin/bash
#
gfortran -c toms792_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms792_prb.f"
  exit
fi
#
gfortran toms792_prb.o -L$HOME/libf77 -ltoms790 -ltoms792
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms792_prb.o"
  exit
fi
rm toms792_prb.o
#
mv a.out toms792_prb
./toms792_prb < toms792_prb_input.txt > toms792_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms792_prb"
  exit
fi
rm toms792_prb
#
echo "Test results written to toms792_prb_output.txt."
