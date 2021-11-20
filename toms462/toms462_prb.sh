#!/bin/bash
#
gfortran -c toms462_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms462_prb.f"
  exit
fi
#
gfortran toms462_prb.o -L$HOME/libf77 -ltoms462
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms462_prb.o"
  exit
fi
rm toms462_prb.o
#
mv a.out toms462_prb
./toms462_prb > toms462_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms462_prb"
  exit
fi
rm toms462_prb
#
echo "Test results written to toms462_prb_output.txt."
