#!/bin/bash
#
gfortran -c toms565_prb3.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms565_prb3.f"
  exit
fi
#
gfortran toms565_prb3.o -L$HOME/libf77 -ltoms565
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms565_prb3.o"
  exit
fi
rm toms565_prb3.o
#
mv a.out toms565_prb3
./toms565_prb3 > toms565_prb3_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms565_prb3"
  exit
fi
rm toms565_prb3
#
echo "Test results written to toms565_prb3_output.txt."
