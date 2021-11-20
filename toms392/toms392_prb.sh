#!/bin/bash
#
gfortran -c toms392_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms392_prb.f"
  exit
fi
#
gfortran toms392_prb.o -L$HOME/libf77 -ltoms392
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms392_prb.o"
  exit
fi
rm toms392_prb.o
#
mv a.out toms392_prb
./toms392_prb > toms392_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms392_prb"
  exit
fi
rm toms392_prb
#
echo "Test results written to toms392_prb_output.txt."
