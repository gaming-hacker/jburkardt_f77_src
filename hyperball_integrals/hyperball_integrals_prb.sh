#!/bin/bash
#
gfortran -c hyperball_integrals_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling hyperball_integrals_prb.f"
  exit
fi
#
gfortran hyperball_integrals_prb.o -L$HOME/libf77 -lhyperball_integrals
if [ $? -ne 0 ]; then
  echo "Errors linking and loading hyperball_integrals_prb.o"
  exit
fi
rm hyperball_integrals_prb.o
#
mv a.out hyperball_integrals_prb
./hyperball_integrals_prb > hyperball_integrals_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running hyperball_integrals_prb"
  exit
fi
rm hyperball_integrals_prb
#
echo "Test program output written to hyperball_integrals_prb_output.txt."
