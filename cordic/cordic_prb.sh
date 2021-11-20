#!/bin/bash
#
gfortran -c cordic_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling cordic_prb.f"
  exit
fi
#
gfortran cordic_prb.o -L$HOME/libf77 -lcordic
if [ $? -ne 0 ]; then
  echo "Errors linking and loading cordic_prb.o"
  exit
fi
rm cordic_prb.o
#
mv a.out cordic_prb
./cordic_prb > cordic_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running cordic_prb"
  exit
fi
rm cordic_prb
#
echo "Test program output written to cordic_prb_output.txt."
