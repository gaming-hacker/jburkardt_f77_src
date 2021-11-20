#!/bin/bash
#
gfortran -c toms511_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms511_prb.f"
  exit
fi
#
gfortran toms511_prb.o -L$HOME/libf77 -ltoms511
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms511_prb.o"
  exit
fi
rm toms511_prb.o
#
mv a.out toms511_prb
./toms511_prb > toms511_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms511_prb"
  exit
fi
rm toms511_prb
#
echo "Test results written to toms511_prb_output.txt."
