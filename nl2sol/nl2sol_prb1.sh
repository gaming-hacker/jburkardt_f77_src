#!/bin/bash
#
gfortran -c nl2sol_prb1.f
if [ $? -ne 0 ]; then
  echo "Errors compiling nl2sol_prb1.f"
  exit
fi
#
gfortran nl2sol_prb1.o -L$HOME/libf77 -lnl2sol
if [ $? -ne 0 ]; then
  echo "Errors linking and loading nl2sol_prb1.o"
  exit
fi
rm nl2sol_prb1.o
#
mv a.out nl2sol_prb1
./nl2sol_prb1 > nl2sol_prb1_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running nl2sol_prb1"
  exit
fi
rm nl2sol_prb1
#
echo "Test results written to nl2sol_prb1_output.txt."
