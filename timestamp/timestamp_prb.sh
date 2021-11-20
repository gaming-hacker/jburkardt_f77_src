#!/bin/bash
#
gfortran -c timestamp_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling timestamp_prb.f"
  exit
fi
#
gfortran timestamp_prb.o -L$HOME/libf77 -ltimestamp
if [ $? -ne 0 ]; then
  echo "Errors linking and loading timestamp_prb.o"
  exit
fi
rm timestamp_prb.o
#
mv a.out timestamp_prb
./timestamp_prb > timestamp_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running timestamp_prb"
  exit
fi
rm timestamp_prb
#
echo "Test results written to timestamp_prb_output.txt."
