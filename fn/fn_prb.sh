#!/bin/bash
#
gfortran -c fn_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling fn_prb.f"
  exit
fi
#
gfortran fn_prb.o -L$HOME/libf77 -lfn -ltest_values
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fn_prb.o"
  exit
fi
rm fn_prb.o
#
mv a.out fn_prb
./fn_prb > fn_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running fn_prb"
  exit
fi
rm fn_prb
#
echo "Test results written to fn_prb_output.txt."
