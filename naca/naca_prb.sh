#!/bin/bash
#
gfortran -c naca_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling naca_prb.f"
  exit
fi
#
gfortran naca_prb.o -L$HOME/libf77 -lnaca
if [ $? -ne 0 ]; then
  echo "Errors linking and loading naca_prb.o"
  exit
fi
rm naca_prb.o
#
mv a.out naca_prb
./naca_prb > naca_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running naca_prb"
  exit
fi
rm naca_prb
#
echo "Test program output written to naca_prb_output.txt."
