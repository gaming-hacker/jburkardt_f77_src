#!/bin/bash
#
gfortran -c toms419_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms419_prb.f"
  exit
fi
#
gfortran toms419_prb.o -L$HOME/libf77 -ltoms419
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms419_prb.o"
  exit
fi
rm toms419_prb.o
#
mv a.out toms419_prb
./toms419_prb > toms419_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms419_prb"
  exit
fi
rm toms419_prb
#
echo "Test results written to toms419_prb_output.txt."
