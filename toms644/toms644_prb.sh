#!/bin/bash
#
gfortran -c toms644_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms644_prb.f"
  exit
fi
#
gfortran toms644_prb.o -L$HOME/libf77 -ltoms644
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms644_prb.o"
  exit
fi
rm toms644_prb.o
#
mv a.out toms644_prb
./toms644_prb > toms644_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms644_prb"
  exit
fi
rm toms644_prb
#
echo "Test results written to toms644_prb_output.txt."
