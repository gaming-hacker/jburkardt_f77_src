#!/bin/bash
#
gfortran -c -g toms614_prb.f >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling toms614_prb.f"
  exit
fi
rm compiler.txt
#
gfortran toms614_prb.o -L$HOME/libf77 -ltoms614
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms614_prb.o"
  exit
fi
rm toms614_prb.o
#
mv a.out toms614_prb
./toms614_prb > toms614_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms614_prb"
  exit
fi
rm toms614_prb
#
echo "Test results written to toms614_prb_output.txt."
