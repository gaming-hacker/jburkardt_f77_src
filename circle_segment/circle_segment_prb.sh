#!/bin/bash
#
gfortran -c circle_segment_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling circle_segment_prb.f"
  exit
fi
#
gfortran circle_segment_prb.o -L$HOME/libf77 -lcircle_segment
if [ $? -ne 0 ]; then
  echo "Errors linking and loading circle_segment_prb.o"
  exit
fi
rm circle_segment_prb.o
#
mv a.out circle_segment_prb
./circle_segment_prb > circle_segment_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running circle_segment_prb"
  exit
fi
rm circle_segment_prb
#
echo "Test program output written to circle_segment_prb_output.txt."
