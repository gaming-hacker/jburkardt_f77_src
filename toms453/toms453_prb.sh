#!/bin/bash
#
gfortran -c toms453_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms453_prb.f"
  exit
fi
#
gfortran toms453_prb.o -L$HOME/libf77 -ltoms453
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms453_prb.o"
  exit
fi
rm toms453_prb.o
#
mv a.out toms453_prb
./toms453_prb > toms453_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms453_prb"
  exit
fi
rm toms453_prb
#
echo "Test results written to toms453_prb_output.txt."
