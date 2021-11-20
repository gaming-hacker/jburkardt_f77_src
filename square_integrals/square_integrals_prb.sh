#!/bin/bash
#
gfortran -c square_integrals_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling square_integrals_prb.f"
  exit
fi
#
gfortran square_integrals_prb.o -L$HOME/libf77 -lsquare_integrals
if [ $? -ne 0 ]; then
  echo "Errors linking and loading square_integrals_prb.o"
  exit
fi
rm square_integrals_prb.o
#
mv a.out square_integrals_prb
./square_integrals_prb > square_integrals_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running square_integrals_prb"
  exit
fi
rm square_integrals_prb
#
echo "Test program output written to square_integrals_prb_output.txt."
