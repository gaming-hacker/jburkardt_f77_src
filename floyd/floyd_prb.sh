#!/bin/bash
#
gfortran -c floyd_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling floyd_prb.f"
  exit
fi
#
gfortran floyd_prb.o -L$HOME/libf77 -lfloyd
if [ $? -ne 0 ]; then
  echo "Errors linking and loading floyd_prb.o"
  exit
fi
rm floyd_prb.o
#
mv a.out floyd_prb
./floyd_prb > floyd_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running floyd_prb"
  exit
fi
rm floyd_prb
#
echo "Program output written to floyd_prb_output.txt"
