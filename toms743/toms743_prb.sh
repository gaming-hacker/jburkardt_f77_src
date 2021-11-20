#!/bin/bash
#
gfortran -c toms743_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms743_prb.f"
  exit
fi
#
gfortran -o toms743_prb toms743_prb.o -L$HOME/libf77 -ltoms743
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms743_prb.o"
  exit
fi
rm toms743_prb.o
#
./toms743_prb > toms743_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms743_prb"
  exit
fi
rm toms743_prb
#
echo "Test results written to toms743_prb_output.txt."
