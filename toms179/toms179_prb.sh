#!/bin/bash
#
gfortran -c toms179_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms179_prb.f"
  exit
fi
#
gfortran toms179_prb.o -L$HOME/libf77 -ltoms179
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms179_prb.o"
  exit
fi
rm toms179_prb.o
#
mv a.out toms179_prb
./toms179_prb > toms179_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms179_prb"
  exit
fi
rm toms179_prb
#
echo "Test results written to toms179_prb_output.txt."
