#!/bin/bash
#
gfortran -c napack_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling napack_prb.f"
  exit
fi
#
gfortran napack_prb.o -L$HOME/libf77 -lnapack
if [ $? -ne 0 ]; then
  echo "Errors linking and loading napack_prb.o"
  exit
fi
rm napack_prb.o
#
mv a.out napack_prb
./napack_prb > napack_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running napack_prb"
  exit
fi
rm napack_prb
#
echo "Program output written to napack_prb_output.txt"
