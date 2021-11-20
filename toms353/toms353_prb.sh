#!/bin/bash
#
gfortran -c toms353_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms353_prb.f"
  exit
fi
#
gfortran toms353_prb.o -L$HOME/libf77 -ltoms353
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms353_prb.o"
  exit
fi
rm toms353_prb.o
#
mv a.out toms353_prb
./toms353_prb > toms353_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms353_prb"
  exit
fi
rm toms353_prb
#
echo "Test results written to toms353_prb_output.txt."
