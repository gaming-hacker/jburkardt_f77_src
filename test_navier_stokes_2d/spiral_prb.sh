#!/bin/bash
#
gfortran -c spiral_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling spiral_prb.f"
  exit
fi
#
gfortran spiral_prb.o -L$HOME/libf77 -lspiral
if [ $? -ne 0 ]; then
  echo "Errors linking and loading spiral_prb.o"
  exit
fi
rm spiral_prb.o
#
mv a.out spiral_prb
./spiral_prb > spiral_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running spiral_prb"
  exit
fi
rm spiral_prb
#
echo "Test program output written to spiral_prb_output.txt."
