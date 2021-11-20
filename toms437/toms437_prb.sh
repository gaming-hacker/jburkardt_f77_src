#!/bin/bash
#
gfortran -c toms437_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms437_prb.f"
  exit
fi
#
gfortran toms437_prb.o -L$HOME/libf77 -ltoms437
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms437_prb.o"
  exit
fi
rm toms437_prb.o
#
mv a.out toms437_prb
./toms437_prb > toms437_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms437_prb"
  exit
fi
rm toms437_prb
#
echo "Test results written to toms437_prb_output.txt."
