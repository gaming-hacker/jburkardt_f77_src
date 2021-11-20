#!/bin/bash
#
gfortran -c odepack_prb5.f
if [ $? -ne 0 ]; then
  echo "Errors compiling odepack_prb5.f"
  exit
fi
#
gfortran odepack_prb5.o -L$HOME/libf77 -lodepack
if [ $? -ne 0 ]; then
  echo "Errors linking and loading odepack_prb5.o"
  exit
fi
rm odepack_prb5.o
#
mv a.out odepack_prb5
./odepack_prb5 > odepack_prb5_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running odepack_prb5"
  exit
fi
rm odepack_prb5
#
echo "Test results written to odepack_prb5_output.txt."
