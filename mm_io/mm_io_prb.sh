#!/bin/bash
#
gfortran -c mm_io_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling mm_io_prb.f"
  exit
fi
#
gfortran mm_io_prb.o -L$HOME/libf77 -lmm_io
if [ $? -ne 0 ]; then
  echo "Errors linking and loading mm_io_prb.o"
  exit
fi
rm mm_io_prb.o
#
mv a.out mm_io_prb
./mm_io_prb > mm_io_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running mm_io_prb"
  exit
fi
rm mm_io_prb
#
echo "Test program output written to mm_io_prb_output.txt."
