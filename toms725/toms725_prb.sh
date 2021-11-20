#!/bin/bash
#
gfortran -c toms725_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms725_prb.f"
  exit
fi
#
gfortran toms725_prb.o -L$HOME/libf77 -ltoms725
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms725_prb.o"
  exit
fi
rm toms725_prb.o
#
mv a.out toms725_prb
./toms725_prb > toms725_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms725_prb"
  exit
fi
rm toms725_prb
#
echo "Test results written to toms725_prb_output.txt."
