#!/bin/bash
#
gfortran -c c8lib_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling c8lib_prb.f"
  exit
fi
#
gfortran c8lib_prb.o -L$HOME/libf77 -lc8lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading c8lib_prb.o"
  exit
fi
rm c8lib_prb.o
#
mv a.out c8lib_prb
./c8lib_prb > c8lib_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running c8lib_prb"
  exit
fi
rm c8lib_prb
#
echo "Test results written to c8lib_prb_output.txt."
