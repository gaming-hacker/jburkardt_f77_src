#!/bin/bash
#
gfortran -c toms467_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms467_prb.f"
  exit
fi
#
gfortran toms467_prb.o -L$HOME/libf77 -ltoms467
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms467_prb.o"
  exit
fi
rm toms467_prb.o
#
mv a.out toms467_prb
./toms467_prb > toms467_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms467_prb"
  exit
fi
rm toms467_prb
#
echo "Test results written to toms467_prb_output.txt."
