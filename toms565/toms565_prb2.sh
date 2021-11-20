#!/bin/bash
#
gfortran -c toms565_prb2.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms565_prb2.f"
  exit
fi
#
gfortran toms565_prb2.o -L$HOME/libf77 -ltoms565
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms565_prb2.o"
  exit
fi
rm toms565_prb2.o
#
mv a.out toms565_prb2
./toms565_prb2 > toms565_prb2_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms565_prb2"
  exit
fi
rm toms565_prb2
#
echo "Test results written to toms565_prb2_output.txt."
