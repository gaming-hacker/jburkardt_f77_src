#!/bin/bash
#
gfortran -c power_method_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling power_method_prb.f"
  exit
fi
#
gfortran power_method_prb.o -L$HOME/libf77 -lpower_method
if [ $? -ne 0 ]; then
  echo "Errors linking and loading power_method_prb.o"
  exit
fi
rm power_method_prb.o
#
mv a.out power_method_prb
./power_method_prb > power_method_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running power_method_prb"
  exit
fi
rm power_method_prb
#
echo "Test results written to power_method_prb_output.txt."
