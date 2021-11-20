#!/bin/bash
#
gfortran -c toms450_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms450_prb.f"
  exit
fi
#
gfortran toms450_prb.o -L$HOME/libf77 -ltoms450
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms450_prb.o"
  exit
fi
rm toms450_prb.o
#
mv a.out toms450_prb
./toms450_prb > toms450_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms450_prb"
  exit
fi
rm toms450_prb
#
echo "Test results written to toms450_prb_output.txt."
