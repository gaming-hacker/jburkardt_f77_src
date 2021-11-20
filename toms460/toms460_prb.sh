#!/bin/bash
#
gfortran -c toms460_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms460_prb.f"
  exit
fi
#
gfortran toms460_prb.o -L$HOME/libf77 -ltoms460
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms460_prb.o"
  exit
fi
rm toms460_prb.o
#
mv a.out toms460_prb
./toms460_prb > toms460_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms460_prb"
  exit
fi
rm toms460_prb
#
echo "Test results written to toms460_prb_output.txt."
