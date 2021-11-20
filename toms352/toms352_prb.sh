#!/bin/bash
#
gfortran -c toms352_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms352_prb.f"
  exit
fi
#
gfortran toms352_prb.o -L$HOME/libf77 -ltoms352
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms352_prb.o"
  exit
fi
rm toms352_prb.o
#
mv a.out toms352_prb
./toms352_prb > toms352_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms352_prb"
  exit
fi
rm toms352_prb
#
echo "Test results written to toms352_prb_output.txt."
