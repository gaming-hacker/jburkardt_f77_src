#!/bin/bash
#
gfortran -c ranlib_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ranlib_prb.f"
  exit
fi
#
gfortran ranlib_prb.o -L$HOME/libf77 -lranlib -lrnglib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ranlib_prb.o"
  exit
fi
rm ranlib_prb.o
#
mv a.out ranlib_prb
./ranlib_prb > ranlib_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ranlib_prb"
  exit
fi
rm ranlib_prb
#
echo "Test results written to ranlib_prb_output.txt."
