#!/bin/bash
#
gfortran -c c4lib_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling c4lib_prb.f"
  exit
fi
#
gfortran c4lib_prb.o -L$HOME/libf77 -lc4lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading c4lib_prb.o"
  exit
fi
rm c4lib_prb.o
#
mv a.out c4lib_prb
./c4lib_prb > c4lib_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running c4lib_prb"
  exit
fi
rm c4lib_prb
#
echo "Test results written to c4lib_prb_output.txt."
