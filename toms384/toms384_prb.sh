#!/bin/bash
#
gfortran -c toms384_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms384_prb.f"
  exit
fi
#
gfortran toms384_prb.o -L$HOME/libf77 -ltoms384
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms384_prb.o"
  exit
fi
rm toms384_prb.o
#
mv a.out toms384_prb
./toms384_prb > toms384_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms384_prb"
  exit
fi
rm toms384_prb
#
echo "Test results written to toms384_prb_output.txt."
