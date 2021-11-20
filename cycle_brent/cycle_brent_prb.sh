#!/bin/bash
#
gfortran -c cycle_brent_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling cycle_brent_prb.f"
  exit
fi
#
gfortran cycle_brent_prb.o -L$HOME/libf77 -lcycle_brent
if [ $? -ne 0 ]; then
  echo "Errors linking and loading cycle_brent_prb.o"
  exit
fi
rm cycle_brent_prb.o
#
mv a.out cycle_brent_prb
./cycle_brent_prb > cycle_brent_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running cycle_brent_prb"
  exit
fi
rm cycle_brent_prb
#
echo "Test program output written to cycle_brent_prb_output.txt."
