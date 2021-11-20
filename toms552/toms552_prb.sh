#!/bin/bash
#
gfortran -c toms552_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms552_prb.f"
  exit
fi
#
gfortran toms552_prb.o -L$HOME/libf77 -ltoms552
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms552_prb.o"
  exit
fi
rm toms552_prb.o
#
mv a.out toms552_prb
./toms552_prb > toms552_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms552_prb"
  exit
fi
rm toms552_prb
#
echo "Test results written to toms552_prb_output.txt."
