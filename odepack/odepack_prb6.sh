#!/bin/bash
#
gfortran -c odepack_prb6.f
if [ $? -ne 0 ]; then
  echo "Errors compiling odepack_prb6.f"
  exit
fi
#
gfortran odepack_prb6.o -L$HOME/libf77 -lodepack
if [ $? -ne 0 ]; then
  echo "Errors linking and loading odepack_prb6.o"
  exit
fi
rm odepack_prb6.o
#
mv a.out odepack_prb6
./odepack_prb6 > odepack_prb6_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running odepack_prb6"
  exit
fi
rm odepack_prb6
#
echo "Test results written to odepack_prb6_output.txt."
