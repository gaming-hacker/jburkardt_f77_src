#!/bin/bash
#
gfortran -c toms418_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling toms418_prb.f"
  exit
fi
#
gfortran toms418_prb.o -L$HOME/libf77 -ltoms418
if [ $? -ne 0 ]; then
  echo "Errors linking and loading toms418_prb.o"
  exit
fi
rm toms418_prb.o
#
mv a.out toms418_prb
./toms418_prb > toms418_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running toms418_prb"
  exit
fi
rm toms418_prb
#
echo "Test results written to toms418_prb_output.txt."
