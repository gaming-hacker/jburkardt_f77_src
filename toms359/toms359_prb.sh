#!/bin/bash
#
gfortran -c toms359_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms359_prb.f"
  exit
fi
#
gfortran toms359_prb.o -L$HOME/libf77 -ltoms359
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms359_prb.o"
  exit
fi
rm toms359_prb.o
#
mv a.out toms359_prb
./toms359_prb > toms359_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms359_prb"
  exit
fi
rm toms359_prb
#
echo "Test results written to toms359_prb_output.txt."
