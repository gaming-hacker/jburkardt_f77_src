#!/bin/bash
#
gfortran -c ball_integrals_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ball_integrals_prb.f"
  exit
fi
#
gfortran ball_integrals_prb.o -L$HOME/libf77 -lball_integrals
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ball_integrals_prb.o"
  exit
fi
rm ball_integrals_prb.o
#
mv a.out ball_integrals_prb
./ball_integrals_prb > ball_integrals_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ball_integrals_prb"
  exit
fi
rm ball_integrals_prb
#
echo "Test program output written to ball_integrals_prb_output.txt."
