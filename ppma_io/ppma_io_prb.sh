#!/bin/bash
#
gfortran -c ppma_io_prb.f
if [ $? -ne 0 ]; then
  echo "Errors compiling ppma_io_prb.f"
  exit
fi
#
gfortran ppma_io_prb.o -L$HOME/libf77 -lppma_io
if [ $? -ne 0 ]; then
  echo "Errors linking and loading ppma_io_prb.o"
  exit
fi
rm ppma_io_prb.o
#
mv a.out ppma_io_prb
./ppma_io_prb > ppma_io_prb_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running ppma_io_prb"
  exit
fi
rm ppma_io_prb
#
echo "Test program output written to ppma_io_prb_output.txt."
