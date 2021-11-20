#!/bin/bash
#
gfortran -c mus_linear_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling mus_linear_prb.f"
  exit
fi
#
gfortran mus_linear_prb.o -L$HOME/libf77 -lmus
if [ $? -ne 0 ]; then
  echo "Errors linking and loading mus_linear_prb.o"
  exit
fi
rm mus_linear_prb.o
#
mv a.out mus_linear_prb
./mus_linear_prb > mus_linear_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running mus_linear_prb"
  exit
fi
rm mus_linear_prb
#
echo "Test results written to mus_linear_prb_output.txt."
