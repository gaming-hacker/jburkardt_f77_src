#!/bin/bash
#
gfortran -c toms097_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms097_prb.f"
  exit
fi
#
gfortran toms097_prb.o -L$HOME/libf77 -ltoms097
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms097_prb.o"
  exit
fi
rm toms097_prb.o
#
mv a.out toms097_prb
./toms097_prb > toms097_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms097_prb"
  exit
fi
rm toms097_prb
#
echo "Test program output written to toms097_prb_output.txt."
