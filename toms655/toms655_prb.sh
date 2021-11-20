#!/bin/bash
#
gfortran -c toms655_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms655_prb.f"
  exit
fi
#
gfortran toms655_prb.o -L$HOME/libf77 -ltoms655
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms655_prb.o"
  exit
fi
rm toms655_prb.o
#
mv a.out toms655_prb
./toms655_prb > toms655_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms655_prb"
  exit
fi
rm toms655_prb
#
echo "Test program output written to toms655_prb_output.txt."
