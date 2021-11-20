#!/bin/bash
#
gfortran -c toms699_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms699_prb.f"
  exit
fi
#
gfortran toms699_prb.o -L$HOME/libf77 -ltoms699
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms699_prb.o"
  exit
fi
rm toms699_prb.o
#
mv a.out toms699_prb
./toms699_prb > toms699_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms699_prb"
  exit
fi
rm toms699_prb
#
echo "Test results written to toms699_prb_output.txt."
