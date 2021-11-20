#!/bin/bash
#
gfortran -c truncated_normal_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling truncated_normal_prb.f"
  exit
fi
#
gfortran truncated_normal_prb.o -L$HOME/libf77 -ltruncated_normal
if [ $? -ne 0 ]; then
  echo "Errors linking and loading truncated_normal_prb.o"
  exit
fi
rm truncated_normal_prb.o
#
mv a.out truncated_normal_prb
./truncated_normal_prb > truncated_normal_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running truncated_normal_prb"
  exit
fi
rm truncated_normal_prb
#
echo "Test program output written to truncated_normal_prb_output.txt."
