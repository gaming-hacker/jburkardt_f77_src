#!/bin/bash
#
gfortran -c toms427_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms427_prb.f"
  exit
fi
#
gfortran toms427_prb.o -L$HOME/libf77 -ltoms427
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms427_prb.o"
  exit
fi
rm toms427_prb.o
#
mv a.out toms427_prb
./toms427_prb > toms427_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms427_prb"
  exit
fi
rm toms427_prb
#
echo "Test results written to toms427_prb_output.txt."
