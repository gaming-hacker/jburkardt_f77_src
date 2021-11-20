#!/bin/bash
#
gfortran -c toms291_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms291_prb.f"
  exit
fi
#
gfortran toms291_prb.o -L$HOME/libf77 -ltoms291
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms291_prb.o"
  exit
fi
rm toms291_prb.o
#
mv a.out toms291_prb
./toms291_prb > toms291_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms291_prb"
  exit
fi
rm toms291_prb
#
echo "Test results written to toms291_prb_output.txt."
