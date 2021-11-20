#!/bin/bash
#
gfortran -c bins_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling bins_prb.f"
  exit
fi
#
gfortran bins_prb.o -L$HOME/libf77 -lbins
if [ $? -ne 0 ]; then
  echo "Errors linking and loading bins_prb.o"
  exit
fi
rm bins_prb.o
#
mv a.out bins_prb
./bins_prb > bins_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running bins_prb"
  exit
fi
rm bins_prb
#
echo "Test results written to bins_prb_output.txt."
