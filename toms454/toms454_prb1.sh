#!/bin/bash
#
gfortran -c toms454_prb1.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms454_prb1.f"
  exit
fi
#
gfortran toms454_prb1.o -L$HOME/libf77 -ltoms454
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms454_prb1.o"
  exit
fi
rm toms454_prb1.o
#
mv a.out toms454_prb1
./toms454_prb1 > toms454_prb1_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms454_prb1"
  exit
fi
rm toms454_prb1
#
echo "Test results written to toms454_prb1_output.txt."
