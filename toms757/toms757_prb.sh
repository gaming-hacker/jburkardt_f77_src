#!/bin/bash
#
gfortran -c toms757_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms757_prb.f"
  exit
fi
#
gfortran toms757_prb.o -L$HOME/libf77 -ltoms757
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms757_prb.o"
  exit
fi
rm toms757_prb.o
#
mv a.out toms757_prb
./toms757_prb > toms757_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms757_prb"
  exit
fi
rm toms757_prb
#
echo "Test results written to toms757_prb_output.txt."
