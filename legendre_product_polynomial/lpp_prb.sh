#!/bin/bash
#
gfortran -c lpp_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling lpp_prb.f"
  exit
fi
#
gfortran -o lpp_prb lpp_prb.o -L$HOME/libf77 -llpp
if [ $? -ne 0 ]; then
  echo "Errors linking and loading lpp_prb.o"
  exit
fi
rm lpp_prb.o
#
./lpp_prb > lpp_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running lpp_prb"
  exit
fi
rm lpp_prb
#
echo "Test results written to lpp_prb_output.txt."
