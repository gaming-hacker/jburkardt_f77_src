#!/bin/bash
#
gfortran -c toms647_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms647_prb.f"
  exit
fi
#
gfortran toms647_prb.o -L$HOME/libf77 -ltoms647
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms647_prb.o"
  exit
fi
rm toms647_prb.o
#
mv a.out toms647_prb
./toms647_prb > toms647_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms647_prb"
  exit
fi
rm toms647_prb
#
echo "Test results written to toms647_prb_output.txt."
