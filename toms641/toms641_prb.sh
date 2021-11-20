#!/bin/bash
#
gfortran -c -g toms641_prb.f >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling toms641_prb.f"
  exit
fi
rm compiler.txt
#
gfortran toms641_prb.o -L$HOME/libf77 -ltoms641
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms641_prb.o"
  exit
fi
rm toms641_prb.o
#
mv a.out toms641_prb
./toms641_prb > toms641_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms641_prb"
  exit
fi
rm toms641_prb
#
echo "Test results written to toms641_prb_output.txt."
