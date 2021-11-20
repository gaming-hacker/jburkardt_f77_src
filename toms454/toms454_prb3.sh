#!/bin/bash
#
gfortran -c toms454_prb3.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms454_prb3.f"
  exit
fi
#
gfortran toms454_prb3.o -L$HOME/libf77 -ltoms454
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms454_prb3.o"
  exit
fi
rm toms454_prb3.o
#
mv a.out toms454_prb3
./toms454_prb3 > toms454_prb3_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms454_prb3"
  exit
fi
rm toms454_prb3
#
echo "Test results written to toms454_prb3_output.txt."
