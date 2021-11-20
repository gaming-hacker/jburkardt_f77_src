#!/bin/bash
#
gfortran -c pink_noise_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling pink_noise_prb.f"
  exit
fi
#
gfortran pink_noise_prb.o -L$HOME/libf77 -lpink_noise
if [ $? -ne 0 ]; then
  echo "Errors linking and loading pink_noise_prb.o"
  exit
fi
rm pink_noise_prb.o
#
mv a.out pink_noise_prb
./pink_noise_prb > pink_noise_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running pink_noise_prb"
  exit
fi
rm pink_noise_prb
#
echo "Test results written to pink_noise_prb_output.txt."
