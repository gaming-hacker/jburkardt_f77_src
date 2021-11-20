#!/bin/bash
#
gfortran -c snakes_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling snakes_prb.f"
  exit
fi
#
gfortran -o snakes_prb snakes_prb.o -L$HOME/libf77 -lsnakes
if [ $? -ne 0 ]; then
  echo "Errors linking and loading snakes_prb.o"
  exit
fi
rm snakes_prb.o
#
./snakes_prb > snakes_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running snakes_prb"
  exit
fi
rm snakes_prb
#
echo "Test program output written to snakes_prb_output.txt."
