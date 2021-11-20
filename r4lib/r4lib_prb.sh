#!/bin/bash
#
gfortran -c r4lib_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling r4lib_prb.f"
  exit
fi
#
gfortran r4lib_prb.o -L$HOME/libf77 -lr4lib
if [ $? -ne 0 ]; then
  echo "Errors linking and loading r4lib_prb.o"
  exit
fi
rm r4lib_prb.o
#
mv a.out r4lib_prb
./r4lib_prb > r4lib_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running r4lib_prb"
  exit
fi
rm r4lib_prb
#
echo "Test results written to r4lib_prb_output.txt."
