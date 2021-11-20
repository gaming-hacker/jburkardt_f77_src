#!/bin/bash
#
gfortran -c toms790_prb1.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms790_prb1.f"
  exit
fi
#
gfortran toms790_prb1.o -L$HOME/libf77 -ltoms790
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms790_prb1.o"
  exit
fi
rm toms790_prb1.o
#
mv a.out toms790_prb1
./toms790_prb1 < toms790_prb1_input.txt > toms790_prb1_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms790_prb1"
  exit
fi
rm toms790_prb1
#
echo "Test results written to toms790_prb1_output.txt."
