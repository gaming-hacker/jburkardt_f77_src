#!/bin/bash
#
gfortran -c latin_random_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling latin_random_prb.f"
  exit
fi
#
gfortran latin_random_prb.o -L$HOME/libf77 -llatin_random
if [ $? -ne 0 ]; then
  echo "Errors linking and loading latin_random_prb.o"
  exit
fi
rm latin_random_prb.o
#
mv a.out latin_random_prb
./latin_random_prb > latin_random_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running latin_random_prb"
  exit
fi
rm latin_random_prb
#
echo "Test program output written to latin_random_prb_output.txt."
