#!/bin/bash
#
gfortran -c colored_noise_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling colored_noise_prb.f"
  exit
fi
#
gfortran colored_noise_prb.o -L$HOME/libf77 -lcolored_noise
if [ $? -ne 0 ]; then
  echo "Errors linking and loading colored_noise_prb.o"
  exit
fi
rm colored_noise_prb.o
#
mv a.out colored_noise_prb
./colored_noise_prb > colored_noise_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running colored_noise_prb"
  exit
fi
rm colored_noise_prb
#
echo "Test results written to colored_noise_prb_output.txt."
