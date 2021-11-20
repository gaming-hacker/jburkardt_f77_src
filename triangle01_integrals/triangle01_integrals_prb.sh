#!/bin/bash
#
gfortran -c triangle01_integrals_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling triangle01_integrals_prb.f"
  exit
fi
#
gfortran triangle01_integrals_prb.o -L$HOME/libf77 -ltriangle01_integrals
if [ $? -ne 0 ]; then
  echo "Errors linking and loading triangle01_integrals_prb.o"
  exit
fi
rm triangle01_integrals_prb.o
#
mv a.out triangle01_integrals_prb
./triangle01_integrals_prb > triangle01_integrals_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running triangle01_integrals_prb"
  exit
fi
rm triangle01_integrals_prb
#
echo "Test program output written to triangle01_integrals_prb_output.txt."
