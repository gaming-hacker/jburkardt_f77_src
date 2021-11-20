#!/bin/bash
#
gfortran -c ode_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ode_prb.f"
  exit
fi
#
gfortran ode_prb.o -L$HOME/libf77 -lode
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ode_prb.o"
  exit
fi
rm ode_prb.o
#
mv a.out ode_prb
./ode_prb > ode_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ode_prb"
  exit
fi
rm ode_prb
#
echo "Test results written to ode_prb_output.txt."
