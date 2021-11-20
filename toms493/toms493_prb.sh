#!/bin/bash
#
gfortran -c toms493_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms493_prb.f"
  exit
fi
#
gfortran toms493_prb.o -L$HOME/libf77 -ltoms493
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms493_prb.o"
  exit
fi
rm toms493_prb.o
#
mv a.out toms493_prb
./toms493_prb > toms493_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms493_prb"
  exit
fi
rm toms493_prb
#
echo "Test results written to toms493_prb_output.txt."
