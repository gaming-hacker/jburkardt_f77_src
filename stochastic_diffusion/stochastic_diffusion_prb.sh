#!/bin/bash
#
gfortran -c stochastic_diffusion_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling stochastic_diffusion_prb.f"
  exit
fi
#
gfortran stochastic_diffusion_prb.o -L$HOME/libf77 -lstochastic_diffusion
if [ $? -ne 0 ]; then
  echo "Errors linking and loading stochastic_diffusion_prb.o"
  exit
fi
rm stochastic_diffusion_prb.o
#
mv a.out stochastic_diffusion_prb
./stochastic_diffusion_prb > stochastic_diffusion_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running stochastic_diffusion_prb"
  exit
fi
rm stochastic_diffusion_prb
#
echo "Test program output written to stochastic_diffusion_prb_output.txt."
