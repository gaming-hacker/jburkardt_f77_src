#!/bin/bash
#
gfortran -c toms358_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms358_prb.f"
  exit
fi
#
gfortran toms358_prb.o -L$HOME/libf77 -ltoms358
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms358_prb.o"
  exit
fi
rm toms358_prb.o
#
mv a.out toms358_prb
./toms358_prb > toms358_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms358_prb"
  exit
fi
rm toms358_prb
#
echo "Test results written to toms358_prb_output.txt."
