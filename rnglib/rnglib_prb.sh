#!/bin/bash
#
gfortran -c rnglib_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling rnglib_prb.f"
  exit
fi
#
gfortran rnglib_prb.o -L$HOME/libf77 -lrnglib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading rnglib_prb.o"
  exit
fi
rm rnglib_prb.o
#
mv a.out rnglib_prb
./rnglib_prb > rnglib_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running rnglib_prb"
  exit
fi
rm rnglib_prb
#
echo "Test results written to rnglib_prb_output.txt."
