#!/bin/bash
#
gfortran -c circle_integrals_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling circle_integrals_prb.f"
  exit
fi
#
gfortran circle_integrals_prb.o -L$HOME/libf77 -lcircle_integrals
if [ $? -ne 0 ]; then
  echo "Errors linking and loading circle_integrals_prb.o"
  exit
fi
rm circle_integrals_prb.o
#
mv a.out circle_integrals_prb
./circle_integrals_prb > circle_integrals_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running circle_integrals_prb"
  exit
fi
rm circle_integrals_prb
#
echo "Test program output written to circle_integrals_prb_output.txt."
