#!/bin/bash
#
gfortran -c toms468_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms468_prb.f"
  exit
fi
#
gfortran toms468_prb.o -L$HOME/libf77 -ltoms468
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms468_prb.o"
  exit
fi
rm toms468_prb.o
#
mv a.out toms468_prb
./toms468_prb > toms468_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms468_prb"
  exit
fi
rm toms468_prb
#
echo "Test results written to toms468_prb_output.txt."
