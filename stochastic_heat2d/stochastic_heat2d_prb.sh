#!/bin/bash
#
gfortran -c stochastic_heat2d_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling stochastic_heat2d_prb.f"
  exit
fi
#
gfortran stochastic_heat2d_prb.o -L$HOME/libf77 -lstochastic_heat2d
if [ $? -ne 0 ]; then
  echo "Errors linking and loading stochastic_heat2d_prb.o"
  exit
fi
rm stochastic_heat2d_prb.o
#
mv a.out stochastic_heat2d_prb
./stochastic_heat2d_prb > stochastic_heat2d_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running stochastic_heat2d_prb"
  exit
fi
rm stochastic_heat2d_prb
#
echo "Test program output written to stochastic_heat2d_prb_output.txt."
