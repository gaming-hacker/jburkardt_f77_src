#!/bin/bash
#
gfortran -c toms322_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms322_prb.f"
  exit
fi
#
gfortran toms322_prb.o -L$HOME/libf77 -ltoms322
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms322_prb.o"
  exit
fi
rm toms322_prb.o
#
mv a.out toms322_prb
./toms322_prb > toms322_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms322_prb"
  exit
fi
rm toms322_prb
#
echo "Test results written to toms322_prb_output.txt."
