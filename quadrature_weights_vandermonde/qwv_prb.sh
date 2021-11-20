#!/bin/bash
#
gfortran -c qwv_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling qwv_prb.f"
  exit
fi
#
gfortran qwv_prb.o -L$HOME/libf77 -lqwv
if [ $? -ne 0 ]; then
  echo "Errors linking and loading qwv_prb.o"
  exit
fi
rm qwv_prb.o
#
mv a.out qwv_prb
./qwv_prb > qwv_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running qwv_prb"
  exit
fi
rm qwv_prb
#
echo "Program output written to qwv_prb_output.txt"
