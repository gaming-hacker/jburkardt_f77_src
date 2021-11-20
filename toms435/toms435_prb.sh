#!/bin/bash
#
gfortran -c toms435_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms435_prb.f"
  exit
fi
#
gfortran toms435_prb.o -L$HOME/libf77 -ltoms435
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms435_prb.o"
  exit
fi
rm toms435_prb.o
#
mv a.out toms435_prb
./toms435_prb > toms435_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms435_prb"
  exit
fi
rm toms435_prb
#
echo "Test results written to toms435_prb_output.txt."
