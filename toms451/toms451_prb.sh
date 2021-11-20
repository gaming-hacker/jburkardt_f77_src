#!/bin/bash
#
gfortran -c toms451_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms451_prb.f"
  exit
fi
#
gfortran toms451_prb.o -L$HOME/libf77 -ltoms451
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms451_prb.o"
  exit
fi
rm toms451_prb.o
#
mv a.out toms451_prb
./toms451_prb > toms451_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms451_prb"
  exit
fi
rm toms451_prb
#
echo "Test results written to toms451_prb_output.txt."
