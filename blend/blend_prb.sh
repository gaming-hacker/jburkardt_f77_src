#!/bin/bash
#
gfortran -c blend_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling blend_prb.f"
  exit
fi
#
gfortran blend_prb.o -L$HOME/libf77 -lblend
if [ $? -ne 0 ]; then
  echo "Errors linking and loading blend_prb.o"
  exit
fi
rm blend_prb.o
#
mv a.out blend_prb
./blend_prb > blend_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running blend_prb"
  exit
fi
rm blend_prb
#
echo "Test program output written to blend_prb_output.txt."
