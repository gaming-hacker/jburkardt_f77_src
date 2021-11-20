#!/bin/bash
#
gfortran -c pppack_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling pppack_prb.f"
  exit
fi
#
gfortran pppack_prb.o -L$HOME/libf77 -lpppack
if [ $? -ne 0 ]; then
  echo "Errors linking and loading pppack_prb.o"
  exit
fi
rm pppack_prb.o
#
mv a.out pppack_prb
./pppack_prb > pppack_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running pppack_prb"
  exit
fi
rm pppack_prb
#
echo "Test results written to pppack_prb_output.txt."
