#!/bin/bash
#
gfortran -c toms793_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms793_prb.f"
  exit
fi
#
gfortran toms793_prb.o -L$HOME/libf77 -ltoms793
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms793_prb.o"
  exit
fi
rm toms793_prb.o
#
mv a.out toms793_prb
./toms793_prb > toms793_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms793_prb"
  exit
fi
rm toms793_prb
#
echo "Test results written to toms793_prb_output.txt."
