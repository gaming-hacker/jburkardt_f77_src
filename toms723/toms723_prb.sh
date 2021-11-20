#!/bin/bash
#
gfortran -c toms723_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms723_prb.f"
  exit
fi
#
gfortran -o toms723_prb toms723_prb.o -L$HOME/libf77 -ltoms723
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms723_prb.o"
  exit
fi
rm toms723_prb.o
#
./toms723_prb > toms723_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms723_prb"
  exit
fi
rm toms723_prb
#
echo "Test results written to toms723_prb_output.txt."
