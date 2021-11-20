#!/bin/bash
#
gfortran -c mus_nonlinear_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling mus_nonlinear_prb.f"
  exit
fi
#
gfortran mus_nonlinear_prb.o -L$HOME/libf77 -lmus
if [ $? -ne 0 ]; then
  echo "Errors linking and loading mus_nonlinear_prb.o"
  exit
fi
rm mus_nonlinear_prb.o
#
mv a.out mus_nonlinear_prb
./mus_nonlinear_prb > mus_nonlinear_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running mus_nonlinear_prb"
  exit
fi
rm mus_nonlinear_prb
#
echo "Test results written to mus_nonlinear_prb_output.txt."
