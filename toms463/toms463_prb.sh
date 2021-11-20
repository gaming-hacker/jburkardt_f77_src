#!/bin/bash
#
gfortran -c toms463_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms463_prb.f"
  exit
fi
#
gfortran toms463_prb.o -L$HOME/libf77 -ltoms463
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms463_prb.o"
  exit
fi
rm toms463_prb.o
#
mv a.out toms463_prb
./toms463_prb > toms463_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms463_prb"
  exit
fi
rm toms463_prb
#
echo "Test results written to toms463_prb_output.txt."
