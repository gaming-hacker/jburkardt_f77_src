#!/bin/bash
#
gfortran -c toms439_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms439_prb.f"
  exit
fi
#
gfortran toms439_prb.o -L$HOME/libf77 -ltoms439
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms439_prb.o"
  exit
fi
rm toms439_prb.o
#
mv a.out toms439_prb
./toms439_prb > toms439_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms439_prb"
  exit
fi
rm toms439_prb
#
echo "Test results written to toms439_prb_output.txt."
