#!/bin/bash
#
gfortran -c solve_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling solve_prb.f"
  exit
fi
#
gfortran solve_prb.o -L$HOME/libf77 -lsolve
if [ $? -ne 0 ]; then
  echo "Errors linking and loading solve_prb.o"
  exit
fi
rm solve_prb.o
#
mv a.out solve_prb
./solve_prb > solve_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running solve_prb"
  exit
fi
rm solve_prb
#
echo "Test program output written to solve_prb_output.txt."
