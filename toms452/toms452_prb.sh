#!/bin/bash
#
gfortran -c toms452_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms452_prb.f"
  exit
fi
#
gfortran toms452_prb.o -L$HOME/libf77 -ltoms452
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms452_prb.o"
  exit
fi
rm toms452_prb.o
#
mv a.out toms452_prb
./toms452_prb > toms452_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms452_prb"
  exit
fi
rm toms452_prb
#
echo "Test results written to toms452_prb_output.txt."
