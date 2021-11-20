#!/bin/bash
#
gfortran -c toms715_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms715_prb.f"
  exit
fi
#
gfortran -o toms715_prb toms715_prb.o -L$HOME/libf77 -ltoms715
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms715_prb.o"
  exit
fi
rm toms715_prb.o
#
./toms715_prb > toms715_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms715_prb"
  exit
fi
rm toms715_prb
#
echo "Test results written to toms715_prb_output.txt."
