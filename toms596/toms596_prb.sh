#!/bin/bash
#
gfortran -c toms596_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms596_prb.f"
  exit
fi
#
gfortran toms596_prb.o -L$HOME/libf77 -ltoms596
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms596_prb.o"
  exit
fi
rm toms596_prb.o
#
mv a.out toms596_prb
./toms596_prb > toms596_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms596_prb"
  exit
fi
rm toms596_prb
#
echo "Test results written to toms596_prb_output.txt."
