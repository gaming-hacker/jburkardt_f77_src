#!/bin/bash
#
gfortran -c nl2sol_prb2.f
if [ $? -ne 0 ]; then
  echo "Errors compiling nl2sol_prb2.f"
  exit
fi
#
gfortran nl2sol_prb2.o -L$HOME/libf77 -lnl2sol
if [ $? -ne 0 ]; then
  echo "Errors linking and loading nl2sol_prb2.o"
  exit
fi
rm nl2sol_prb2.o
#
mv a.out nl2sol_prb2
./nl2sol_prb2 > nl2sol_prb2_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running nl2sol_prb2"
  exit
fi
rm nl2sol_prb2
#
echo "Test results written to nl2sol_prb2_output.txt."
