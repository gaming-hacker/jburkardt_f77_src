#!/bin/bash
#
gfortran -c toms446_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms446_prb.f"
  exit
fi
#
gfortran toms446_prb.o -L$HOME/libf77 -ltoms446
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms446_prb.o"
  exit
fi
rm toms446_prb.o
#
mv a.out toms446_prb
./toms446_prb > toms446_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms446_prb"
  exit
fi
rm toms446_prb
#
echo "Test results written to toms446_prb_output.txt."
