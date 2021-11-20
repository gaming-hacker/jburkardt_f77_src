#!/bin/bash
#
gfortran -c lebesgue_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling lebesgue_prb.f"
  exit
fi
#
gfortran lebesgue_prb.o -L$HOME/libf77 -llebesgue
if [ $? -ne 0 ]; then
  echo "Errors linking and loading lebesgue_prb.o"
  exit
fi
rm lebesgue_prb.o
#
mv a.out lebesgue_prb
./lebesgue_prb > lebesgue_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running lebesgue_prb"
  exit
fi
rm lebesgue_prb
#
echo "Test program output written to lebesgue_prb_output.txt."
