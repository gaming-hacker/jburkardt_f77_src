#!/bin/bash
#
gfortran -c toms724_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms724_prb.f"
  exit
fi
#
gfortran toms724_prb.o -L$HOME/libf77 -ltoms724
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms724_prb.o"
  exit
fi
rm toms724_prb.o
#
mv a.out toms724_prb
./toms724_prb > toms724_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms724_prb"
  exit
fi
rm toms724_prb
#
echo "Test results written to toms724_prb_output.txt."
