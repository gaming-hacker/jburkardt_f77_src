#!/bin/bash
#
gfortran -c toms344_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms344_prb.f"
  exit
fi
#
gfortran toms344_prb.o -L$HOME/libf77 -ltoms344
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms344_prb.o"
  exit
fi
rm toms344_prb.o
#
mv a.out toms344_prb
./toms344_prb > toms344_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms344_prb"
  exit
fi
rm toms344_prb
#
echo "Test results written to toms344_prb_output.txt."
