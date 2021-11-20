#!/bin/bash
#
gfortran -c toms655_original_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms655_original_prb.f"
  exit
fi
#
gfortran toms655_original_prb.o -L$HOME/libf77 -ltoms655_original
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms655_original_prb.o"
  exit
fi
rm toms655_original_prb.o
#
mv a.out toms655_original_prb
./toms655_original_prb > toms655_original_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms655_original_prb"
  exit
fi
rm toms655_original_prb
#
echo "Test results written to toms655_original_prb_output.txt."
