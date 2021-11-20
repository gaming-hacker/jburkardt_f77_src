#!/bin/bash
#
gfortran -c gnufor_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling gnufor_prb.f"
  exit
fi
#
gfortran gnufor_prb.o -L$HOME/libf77 -lgnufor
if [ $? -ne 0 ]; then
  echo "Errors linking and loading gnufor_prb.o"
  exit
fi
rm gnufor_prb.o
#
mv a.out gnufor_prb
./gnufor_prb > gnufor_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running gnufor_prb"
  exit
fi
rm gnufor_prb
#
echo "Test program output written to gnufor_prb_output.txt."
