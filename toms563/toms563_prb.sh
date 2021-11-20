#!/bin/bash
#
gfortran -c toms563_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms563_prb.f"
  exit
fi
#
gfortran toms563_prb.o -L$HOME/libf77 -ltoms563
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms563_prb.o"
  exit
fi
rm toms563_prb.o
#
mv a.out toms563_prb
./toms563_prb < toms563_prb_input.txt > toms563_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms563_prb"
  exit
fi
rm toms563_prb
#
echo "Test results written to toms563_prb_output.txt."
