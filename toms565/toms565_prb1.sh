#!/bin/bash
#
gfortran -c toms565_prb1.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms565_prb1.f"
  exit
fi
#
gfortran toms565_prb1.o -L$HOME/libf77 -ltoms565
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms565_prb1.o"
  exit
fi
rm toms565_prb1.o
#
mv a.out toms565_prb1
./toms565_prb1 > toms565_prb1_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms565_prb1"
  exit
fi
rm toms565_prb1
#
echo "Test results written to toms565_prb1_output.txt."
