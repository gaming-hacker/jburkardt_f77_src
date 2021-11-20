#!/bin/bash
#
gfortran -c qwv_2d_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling qwv_2d_prb.f"
  exit
fi
#
gfortran qwv_2d_prb.o -L$HOME/libf77 -lqwv_2d
if [ $? -ne 0 ]; then
  echo "Errors linking and loading qwv_2d_prb.o"
  exit
fi
rm qwv_2d_prb.o
#
mv a.out qwv_2d_prb
./qwv_2d_prb > qwv_2d_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running qwv_2d_prb"
  exit
fi
rm qwv_2d_prb
#
echo "Program output written to qwv_2d_prb_output.txt"
