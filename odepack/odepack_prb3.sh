#!/bin/bash
#
gfortran -c odepack_prb3.f
if [ $? -ne 0 ]; then
  echo "Errors compiling odepack_prb3.f"
  exit
fi
#
gfortran odepack_prb3.o -L$HOME/libf77 -lodepack
if [ $? -ne 0 ]; then
  echo "Errors linking and loading odepack_prb3.o"
  exit
fi
rm odepack_prb3.o
#
mv a.out odepack_prb3
./odepack_prb3 > odepack_prb3_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running odepack_prb3"
  exit
fi
rm odepack_prb3
#
echo "Test results written to odepack_prb3_output.txt."
