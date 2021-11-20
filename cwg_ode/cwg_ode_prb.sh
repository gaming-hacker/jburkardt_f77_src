#!/bin/bash
#
gfortran -c cwg_ode_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling cwg_ode_prb.f"
  exit
fi
#
gfortran cwg_ode_prb.o -L$HOME/libf77 -lcwg_ode
if [ $? -ne 0 ]; then
  echo "Errors linking and loading cwg_ode_prb.o"
  exit
fi
rm cwg_ode_prb.o
#
mv a.out cwg_ode_prb
./cwg_ode_prb > cwg_ode_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running cwg_ode_prb"
  exit
fi
rm cwg_ode_prb
#
echo "Test results written to cwg_ode_prb_output.txt."
