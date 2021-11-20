#!/bin/bash
#
gfortran -c toms599_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms599_prb.f"
  exit
fi
#
gfortran toms599_prb.o -L$HOME/libf77 -ltoms599
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms599_prb.o"
  exit
fi
rm toms599_prb.o
#
mv a.out toms599_prb
./toms599_prb > toms599_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms599_prb"
  exit
fi
rm toms599_prb
#
echo "Test results written to toms599_prb_output.txt."
