#!/bin/bash
#
gfortran -c toms632_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms632_prb.f"
  exit
fi
#
gfortran toms632_prb.o -L$HOME/libf77 -ltoms632
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms632_prb.o"
  exit
fi
rm toms632_prb.o
#
mv a.out toms632_prb
./toms632_prb < toms632_prb_input.txt > toms632_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms632_prb"
  exit
fi
rm toms632_prb
#
echo "Test results written to toms632_prb_output.txt."
