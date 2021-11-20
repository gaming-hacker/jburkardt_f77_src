#!/bin/bash
#
gfortran -c toms438_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms438_prb.f"
  exit
fi
#
gfortran toms438_prb.o -L$HOME/libf77 -ltoms438
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms438_prb.o"
  exit
fi
rm toms438_prb.o
#
mv a.out toms438_prb
./toms438_prb > toms438_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms438_prb"
  exit
fi
rm toms438_prb
#
echo "Test results written to toms438_prb_output.txt."
