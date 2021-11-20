#!/bin/bash
#
gfortran -c toms611_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms611_prb.f"
  exit
fi
#
gfortran toms611_prb.o -L$HOME/libf77 -ltoms611
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms611_prb.o"
  exit
fi
rm toms611_prb.o
#
mv a.out toms611_prb
./toms611_prb > toms611_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms611_prb"
  exit
fi
rm toms611_prb
#
echo "Test results written to toms611_prb_output.txt."
