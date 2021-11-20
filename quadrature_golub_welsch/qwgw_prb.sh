#!/bin/bash
#
gfortran -c qwgw_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling qwgw_prb.f"
  exit
fi
#
gfortran qwgw_prb.o -L$HOME/libf77 -lqwgw
if [ $? -ne 0 ]; then
  echo "Errors linking and loading qwgw_prb.o"
  exit
fi
rm qwgw_prb.o
#
mv a.out qwgw_prb
./qwgw_prb > qwgw_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running qwgw_prb"
  exit
fi
rm qwgw_prb
#
echo "Program output written to qwgw_prb_output.txt"
